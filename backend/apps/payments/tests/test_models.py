"""
Tests for Payment, Wallet, WalletTransaction, Withdrawal, and PaymentMethod models.
"""
import pytest
from decimal import Decimal
from apps.payments.models import (
    Payment, Wallet, BankAccount, WalletTransaction, Withdrawal, PaymentMethod
)


@pytest.mark.django_db
class TestPaymentModel:
    """Tests for the Payment model."""

    def test_create_payment(self, booking_factory):
        booking = booking_factory()
        payment = Payment.objects.create(
            booking=booking,
            provider='stripe',
            status='initiated',
            amount=booking.grand_total,
            currency='USD',
        )
        assert payment.provider == 'stripe'
        assert payment.status == 'initiated'
        assert payment.amount == booking.grand_total

    def test_provider_choices(self, booking_factory):
        booking = booking_factory()
        for provider in ['paynow', 'payfast', 'paypal', 'ozow', 'stripe', 'flutterwave', 'cash_on_arrival']:
            payment = Payment.objects.create(
                booking=booking,
                provider=provider,
                status='initiated',
                amount=Decimal('100.00'),
            )
            assert payment.provider == provider

    def test_status_choices(self, booking_factory):
        booking = booking_factory()
        for status_val in ['initiated', 'success', 'failed']:
            payment = Payment.objects.create(
                booking=booking,
                provider='stripe',
                status=status_val,
                amount=Decimal('100.00'),
            )
            assert payment.status == status_val

    def test_str_representation(self, booking_factory):
        booking = booking_factory()
        payment = Payment.objects.create(
            booking=booking,
            provider='paynow',
            status='success',
            amount=Decimal('150.00'),
        )
        assert str(payment) is not None


@pytest.mark.django_db
class TestWalletModel:
    """Tests for the Wallet model."""

    def test_create_wallet(self, user_factory):
        user = user_factory()
        wallet = Wallet.objects.create(user=user)
        assert wallet.balance == Decimal('0.00')
        assert wallet.status == 'active'

    def test_one_to_one_user(self, user_factory):
        user = user_factory()
        Wallet.objects.create(user=user)
        with pytest.raises(Exception):
            Wallet.objects.create(user=user)

    def test_str_representation(self, user_factory):
        user = user_factory(email='wallet@test.com')
        wallet = Wallet.objects.create(user=user)
        assert 'wallet@test.com' in str(wallet)


@pytest.mark.django_db
class TestBankAccountModel:
    """Tests for the BankAccount model."""

    def test_create_bank_account(self, user_factory):
        user = user_factory()
        account = BankAccount.objects.create(
            user=user,
            bank_name='FNB',
            account_name='Test User',
            account_number='1234567890',
        )
        assert account.bank_name == 'FNB'
        assert account.is_primary is False

    def test_primary_account(self, user_factory):
        user = user_factory()
        account = BankAccount.objects.create(
            user=user,
            bank_name='FNB',
            account_name='Test',
            account_number='123',
            is_primary=True,
        )
        assert account.is_primary is True


@pytest.mark.django_db
class TestWalletTransactionModel:
    """Tests for the WalletTransaction model."""

    def test_create_credit_transaction(self, user_factory):
        user = user_factory()
        wallet = Wallet.objects.create(user=user)
        txn = WalletTransaction.objects.create(
            wallet=wallet,
            txn_type='credit',
            amount=Decimal('100.00'),
            description='Booking payout',
        )
        assert txn.txn_type == 'credit'
        assert txn.amount == Decimal('100.00')
        assert txn.reference is not None

    def test_auto_generated_reference(self, user_factory):
        user = user_factory()
        wallet = Wallet.objects.create(user=user)
        txn = WalletTransaction.objects.create(
            wallet=wallet,
            txn_type='credit',
            amount=Decimal('50.00'),
        )
        assert txn.reference.startswith('TXN')

    def test_metadata_default(self, user_factory):
        user = user_factory()
        wallet = Wallet.objects.create(user=user)
        txn = WalletTransaction.objects.create(
            wallet=wallet,
            txn_type='debit',
            amount=Decimal('25.00'),
        )
        assert txn.metadata == {}


@pytest.mark.django_db
class TestWithdrawalModel:
    """Tests for the Withdrawal model."""

    def test_create_withdrawal(self, user_factory):
        user = user_factory()
        wallet = Wallet.objects.create(user=user, balance=Decimal('500.00'))
        bank = BankAccount.objects.create(
            user=user, bank_name='FNB', account_name='Test', account_number='123',
        )
        withdrawal = Withdrawal.objects.create(
            wallet=wallet,
            bank_account=bank,
            amount=Decimal('100.00'),
        )
        assert withdrawal.status == 'pending'
        assert withdrawal.reference is not None

    def test_status_choices(self, user_factory):
        user = user_factory()
        wallet = Wallet.objects.create(user=user, balance=Decimal('500.00'))
        bank = BankAccount.objects.create(
            user=user, bank_name='FNB', account_name='Test', account_number='123',
        )
        for status_val in ['pending', 'processing', 'completed', 'failed']:
            w = Withdrawal.objects.create(
                wallet=wallet, bank_account=bank,
                amount=Decimal('10.00'), status=status_val,
            )
            assert w.status == status_val


@pytest.mark.django_db
class TestPaymentMethodModel:
    """Tests for the PaymentMethod model."""

    def test_create_payment_method(self, user_factory):
        user = user_factory()
        method = PaymentMethod.objects.create(
            user=user,
            provider='stripe',
            provider_token='tok_test123',
            display_name='Visa ending in 4242',
            card_brand='visa',
            card_last4='4242',
        )
        assert method.provider == 'stripe'
        assert method.is_default is False
        assert method.deleted_at is None

    def test_soft_delete(self, user_factory):
        user = user_factory()
        method = PaymentMethod.objects.create(
            user=user,
            provider='stripe',
            provider_token='tok_test456',
            display_name='Visa 1234',
        )
        method.soft_delete()
        assert method.deleted_at is not None

    def test_default_enforcement(self, user_factory):
        user = user_factory()
        m1 = PaymentMethod.objects.create(
            user=user,
            provider='stripe',
            provider_token='tok_1',
            display_name='Card 1',
            is_default=True,
        )
        m2 = PaymentMethod.objects.create(
            user=user,
            provider='paynow',
            provider_token='tok_2',
            display_name='Card 2',
            is_default=True,
        )
        # After save, m2 should be default and m1 should not
        m1.refresh_from_db()
        assert m2.is_default is True
        assert m1.is_default is False
