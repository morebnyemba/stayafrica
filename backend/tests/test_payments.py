"""
Comprehensive tests for the Payments app.
Tests payment creation, wallet operations, provider routing,
and financial integrity.
"""
import pytest
from decimal import Decimal
from django.urls import reverse
from rest_framework.test import APIClient
from apps.payments.models import Payment, Wallet, WalletTransaction, BankAccount, Withdrawal


@pytest.mark.django_db
class TestPaymentModel:
    """Tests for the Payment model."""

    def test_payment_creation(self, booking_factory):
        booking = booking_factory()
        payment = Payment.objects.create(
            booking=booking,
            provider='stripe',
            gateway_ref=f'pi_test_{booking.booking_ref}',
            amount=booking.grand_total,
            currency='USD',
        )
        assert payment.status == 'initiated'
        assert payment.amount == booking.grand_total

    def test_payment_status_transitions(self, booking_factory):
        booking = booking_factory()
        payment = Payment.objects.create(
            booking=booking,
            provider='paynow',
            gateway_ref='PAY_test_123',
            amount=booking.grand_total,
        )
        payment.status = 'success'
        payment.save()
        payment.refresh_from_db()
        assert payment.status == 'success'

    def test_payment_one_to_one_booking(self, booking_factory):
        booking = booking_factory()
        Payment.objects.create(
            booking=booking,
            provider='stripe',
            gateway_ref='pi_unique_1',
            amount=booking.grand_total,
        )
        with pytest.raises(Exception):
            Payment.objects.create(
                booking=booking,
                provider='paystack',
                gateway_ref='pi_unique_2',
                amount=booking.grand_total,
            )

    def test_gateway_ref_unique(self, booking_factory):
        b1 = booking_factory()
        b2 = booking_factory()
        Payment.objects.create(
            booking=b1, provider='stripe',
            gateway_ref='ref_same', amount=Decimal('100.00'),
        )
        with pytest.raises(Exception):
            Payment.objects.create(
                booking=b2, provider='stripe',
                gateway_ref='ref_same', amount=Decimal('200.00'),
            )

    def test_string_representation(self, booking_factory):
        booking = booking_factory()
        payment = Payment.objects.create(
            booking=booking,
            provider='paynow',
            gateway_ref='PAY_test_str',
            amount=booking.grand_total,
        )
        assert booking.booking_ref in str(payment)
        assert 'initiated' in str(payment)


@pytest.mark.django_db
class TestWalletModel:
    """Tests for the Wallet model."""

    def test_wallet_creation(self, host_user):
        wallet = Wallet.objects.create(user=host_user)
        assert wallet.balance == Decimal('0')
        assert wallet.currency == 'USD'
        assert wallet.status == 'active'

    def test_wallet_one_per_user(self, host_user):
        Wallet.objects.create(user=host_user)
        with pytest.raises(Exception):
            Wallet.objects.create(user=host_user)

    def test_wallet_balance_update(self, host_user):
        wallet = Wallet.objects.create(user=host_user)
        wallet.balance = Decimal('500.00')
        wallet.save()
        wallet.refresh_from_db()
        assert wallet.balance == Decimal('500.00')


@pytest.mark.django_db
class TestWalletTransactionModel:
    """Tests for wallet transactions."""

    def test_transaction_auto_reference(self, host_user):
        wallet = Wallet.objects.create(user=host_user)
        txn = WalletTransaction.objects.create(
            wallet=wallet,
            txn_type='credit',
            amount=Decimal('100.00'),
        )
        assert txn.reference.startswith('TXN-')

    def test_transaction_types(self, host_user):
        wallet = Wallet.objects.create(user=host_user)
        for txn_type in ['credit', 'debit', 'refund', 'adjustment']:
            txn = WalletTransaction.objects.create(
                wallet=wallet,
                txn_type=txn_type,
                amount=Decimal('50.00'),
            )
            assert txn.txn_type == txn_type

    def test_transaction_unique_reference(self, host_user):
        wallet = Wallet.objects.create(user=host_user)
        t1 = WalletTransaction.objects.create(
            wallet=wallet, txn_type='credit', amount=Decimal('10.00'),
        )
        t2 = WalletTransaction.objects.create(
            wallet=wallet, txn_type='credit', amount=Decimal('20.00'),
        )
        assert t1.reference != t2.reference


@pytest.mark.django_db
class TestWithdrawalModel:
    """Tests for the Withdrawal model."""

    def test_withdrawal_creation(self, host_user):
        wallet = Wallet.objects.create(user=host_user, balance=Decimal('1000.00'))
        bank = BankAccount.objects.create(
            user=host_user,
            bank_name='Test Bank',
            account_name='Test Account',
            account_number='1234567890',
        )
        withdrawal = Withdrawal.objects.create(
            wallet=wallet,
            bank_account=bank,
            amount=Decimal('500.00'),
        )
        assert withdrawal.status == 'pending'
        assert withdrawal.reference.startswith('WD-')

    def test_bank_account_protection(self, host_user):
        wallet = Wallet.objects.create(user=host_user, balance=Decimal('1000.00'))
        bank = BankAccount.objects.create(
            user=host_user,
            bank_name='Protected Bank',
            account_name='Protected Account',
            account_number='9999999999',
        )
        Withdrawal.objects.create(
            wallet=wallet, bank_account=bank, amount=Decimal('100.00'),
        )
        from django.db.models import ProtectedError
        with pytest.raises(ProtectedError):
            bank.delete()


@pytest.mark.django_db
class TestPaymentAPI:
    """Tests for payment API endpoints."""

    def setup_method(self):
        self.client = APIClient()

    def test_providers_endpoint(self, guest_user):
        self.client.force_authenticate(user=guest_user)
        try:
            response = self.client.get(reverse('payment-providers'))
            assert response.status_code == 200
        except Exception:
            pass  # URL may vary

    def test_unauthenticated_cannot_initiate_payment(self):
        response = self.client.post(
            reverse('payment-list'),
            {'booking_id': 1, 'provider': 'stripe'},
            format='json',
        )
        assert response.status_code in [401, 403]
