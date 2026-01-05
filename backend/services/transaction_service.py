"""
Transaction Service
Handles wallet transactions with ACID guarantees and rollback capabilities
"""
from decimal import Decimal
from django.db import transaction, IntegrityError
from django.utils import timezone
from apps.payments.models import Wallet, WalletTransaction, Payment, Withdrawal
from apps.bookings.models import Booking
import logging
import uuid

logger = logging.getLogger(__name__)


class TransactionError(Exception):
    """Base exception for transaction errors"""
    pass


class InsufficientBalanceError(TransactionError):
    """Raised when wallet has insufficient balance"""
    pass


class TransactionService:
    """
    Service for handling wallet transactions with atomicity and rollback support
    """
    
    @staticmethod
    @transaction.atomic
    def create_wallet(user, currency='USD'):
        """
        Create a wallet for a user
        """
        try:
            wallet, created = Wallet.objects.get_or_create(
                user=user,
                defaults={'currency': currency, 'balance': Decimal('0.00')}
            )
            if created:
                logger.info(f"Created wallet for user {user.id}")
            return wallet
        except IntegrityError as e:
            logger.error(f"Failed to create wallet for user {user.id}: {str(e)}")
            raise TransactionError(f"Failed to create wallet: {str(e)}")
    
    @staticmethod
    @transaction.atomic
    def credit_wallet(wallet, amount, booking=None, metadata=None, reference=None):
        """
        Credit amount to wallet with transaction record
        Returns the transaction object
        """
        if wallet.status != 'active':
            raise TransactionError('Wallet is not active')
        
        amount = Decimal(str(amount))
        if amount <= 0:
            raise TransactionError('Credit amount must be positive')
        
        # Create transaction record
        txn = WalletTransaction.objects.create(
            wallet=wallet,
            booking=booking,
            txn_type='credit',
            status='pending',
            amount=amount,
            currency=wallet.currency,
            reference=reference or f'CR-{uuid.uuid4().hex[:12].upper()}',
            metadata=metadata or {}
        )
        
        try:
            # Update wallet balance
            wallet.balance += amount
            wallet.save(update_fields=['balance', 'updated_at'])
            
            # Mark transaction as completed
            txn.status = 'completed'
            txn.save(update_fields=['status', 'updated_at'])
            
            logger.info(f"Credited {amount} to wallet {wallet.id}, txn: {txn.reference}")
            return txn
            
        except Exception as e:
            # Rollback transaction status
            txn.status = 'failed'
            txn.save(update_fields=['status', 'updated_at'])
            logger.error(f"Failed to credit wallet {wallet.id}: {str(e)}")
            raise TransactionError(f"Failed to credit wallet: {str(e)}")
    
    @staticmethod
    @transaction.atomic
    def debit_wallet(wallet, amount, booking=None, metadata=None, reference=None):
        """
        Debit amount from wallet with transaction record
        Returns the transaction object
        """
        if wallet.status != 'active':
            raise TransactionError('Wallet is not active')
        
        amount = Decimal(str(amount))
        if amount <= 0:
            raise TransactionError('Debit amount must be positive')
        
        # Check balance
        if wallet.balance < amount:
            raise InsufficientBalanceError(
                f'Insufficient balance. Available: {wallet.balance}, Required: {amount}'
            )
        
        # Create transaction record
        txn = WalletTransaction.objects.create(
            wallet=wallet,
            booking=booking,
            txn_type='debit',
            status='pending',
            amount=amount,
            currency=wallet.currency,
            reference=reference or f'DB-{uuid.uuid4().hex[:12].upper()}',
            metadata=metadata or {}
        )
        
        try:
            # Update wallet balance
            wallet.balance -= amount
            wallet.save(update_fields=['balance', 'updated_at'])
            
            # Mark transaction as completed
            txn.status = 'completed'
            txn.save(update_fields=['status', 'updated_at'])
            
            logger.info(f"Debited {amount} from wallet {wallet.id}, txn: {txn.reference}")
            return txn
            
        except Exception as e:
            # Rollback transaction status
            txn.status = 'failed'
            txn.save(update_fields=['status', 'updated_at'])
            logger.error(f"Failed to debit wallet {wallet.id}: {str(e)}")
            raise TransactionError(f"Failed to debit wallet: {str(e)}")
    
    @staticmethod
    @transaction.atomic
    def reverse_transaction(txn_reference, reason=''):
        """
        Reverse a completed transaction
        Creates a reversal transaction and updates wallet balance
        """
        try:
            # Find original transaction
            original_txn = WalletTransaction.objects.select_for_update().get(
                reference=txn_reference,
                status='completed'
            )
        except WalletTransaction.DoesNotExist:
            raise TransactionError(f'Transaction {txn_reference} not found or not completed')
        
        wallet = original_txn.wallet
        
        # Create reversal transaction
        reversal_type = 'credit' if original_txn.txn_type == 'debit' else 'debit'
        reversal_txn = WalletTransaction.objects.create(
            wallet=wallet,
            booking=original_txn.booking,
            txn_type=reversal_type,
            status='pending',
            amount=original_txn.amount,
            currency=wallet.currency,
            reference=f'REV-{original_txn.reference}',
            metadata={
                'reason': reason,
                'original_transaction': original_txn.reference,
                'reversal': True
            }
        )
        
        try:
            # Update wallet balance (opposite of original)
            if original_txn.txn_type == 'debit':
                wallet.balance += original_txn.amount
            else:
                if wallet.balance < original_txn.amount:
                    raise InsufficientBalanceError('Insufficient balance for reversal')
                wallet.balance -= original_txn.amount
            
            wallet.save(update_fields=['balance', 'updated_at'])
            
            # Mark transactions
            reversal_txn.status = 'completed'
            reversal_txn.save(update_fields=['status', 'updated_at'])
            
            original_txn.status = 'reversed'
            original_txn.save(update_fields=['status', 'updated_at'])
            
            logger.info(f"Reversed transaction {txn_reference}, new txn: {reversal_txn.reference}")
            return reversal_txn
            
        except Exception as e:
            reversal_txn.status = 'failed'
            reversal_txn.save(update_fields=['status', 'updated_at'])
            logger.error(f"Failed to reverse transaction {txn_reference}: {str(e)}")
            raise TransactionError(f"Failed to reverse transaction: {str(e)}")
    
    @staticmethod
    @transaction.atomic
    def process_booking_payment(booking, payment):
        """
        Process booking payment and credit host wallet
        Handles commission deduction and platform fees
        """
        if payment.status != 'success':
            raise TransactionError('Payment must be successful')
        
        # Get or create host wallet
        host = booking.rental_property.host
        host_wallet = TransactionService.create_wallet(host, booking.currency)
        
        # Calculate host payout (total - commission)
        host_payout = booking.grand_total - booking.commission_fee
        
        # Credit host wallet
        credit_txn = TransactionService.credit_wallet(
            wallet=host_wallet,
            amount=host_payout,
            booking=booking,
            metadata={
                'payment_id': payment.id,
                'booking_ref': booking.booking_ref,
                'total_amount': str(booking.grand_total),
                'commission_fee': str(booking.commission_fee),
                'host_payout': str(host_payout)
            },
            reference=f'PAY-{payment.gateway_ref}'
        )
        
        logger.info(
            f"Processed booking payment: booking={booking.booking_ref}, "
            f"host_payout={host_payout}, txn={credit_txn.reference}"
        )
        
        return credit_txn
    
    @staticmethod
    @transaction.atomic
    def process_refund(booking, amount, reason=''):
        """
        Process refund for a booking
        Debits host wallet and creates refund transaction
        """
        host = booking.rental_property.host
        
        try:
            host_wallet = Wallet.objects.select_for_update().get(user=host)
        except Wallet.DoesNotExist:
            raise TransactionError('Host wallet not found')
        
        # Debit from host wallet
        refund_txn = WalletTransaction.objects.create(
            wallet=host_wallet,
            booking=booking,
            txn_type='refund',
            status='pending',
            amount=Decimal(str(amount)),
            currency=booking.currency,
            reference=f'REF-{booking.booking_ref}-{uuid.uuid4().hex[:8].upper()}',
            metadata={
                'reason': reason,
                'booking_ref': booking.booking_ref
            }
        )
        
        try:
            # Check and update balance
            if host_wallet.balance < amount:
                raise InsufficientBalanceError('Insufficient balance for refund')
            
            host_wallet.balance -= Decimal(str(amount))
            host_wallet.save(update_fields=['balance', 'updated_at'])
            
            refund_txn.status = 'completed'
            refund_txn.save(update_fields=['status', 'updated_at'])
            
            logger.info(f"Processed refund: {amount} for booking {booking.booking_ref}")
            return refund_txn
            
        except Exception as e:
            refund_txn.status = 'failed'
            refund_txn.save(update_fields=['status', 'updated_at'])
            logger.error(f"Failed to process refund: {str(e)}")
            raise TransactionError(f"Failed to process refund: {str(e)}")
    
    @staticmethod
    @transaction.atomic
    def initiate_withdrawal(wallet, bank_account, amount):
        """
        Initiate withdrawal from wallet to bank account
        Locks the amount in wallet by creating a pending withdrawal
        """
        if wallet.status != 'active':
            raise TransactionError('Wallet is not active')
        
        amount = Decimal(str(amount))
        
        # Check balance
        if wallet.balance < amount:
            raise InsufficientBalanceError(
                f'Insufficient balance. Available: {wallet.balance}'
            )
        
        # Create withdrawal record
        withdrawal = Withdrawal.objects.create(
            wallet=wallet,
            bank_account=bank_account,
            amount=amount,
            currency=wallet.currency,
            status='pending',
            notes=''
        )
        
        # Create debit transaction
        debit_txn = TransactionService.debit_wallet(
            wallet=wallet,
            amount=amount,
            metadata={
                'withdrawal_id': withdrawal.id,
                'withdrawal_ref': withdrawal.reference,
                'bank_account': bank_account.account_number
            },
            reference=f'WD-{withdrawal.reference}'
        )
        
        logger.info(f"Initiated withdrawal: {withdrawal.reference}, amount: {amount}")
        return withdrawal
    
    @staticmethod
    @transaction.atomic
    def complete_withdrawal(withdrawal_id, notes=''):
        """
        Mark withdrawal as completed
        """
        try:
            withdrawal = Withdrawal.objects.select_for_update().get(id=withdrawal_id)
        except Withdrawal.DoesNotExist:
            raise TransactionError('Withdrawal not found')
        
        if withdrawal.status != 'pending':
            raise TransactionError(f'Withdrawal is not pending (status: {withdrawal.status})')
        
        withdrawal.status = 'completed'
        withdrawal.processed_at = timezone.now()
        withdrawal.notes = notes
        withdrawal.save(update_fields=['status', 'processed_at', 'notes', 'updated_at'])
        
        logger.info(f"Completed withdrawal: {withdrawal.reference}")
        return withdrawal
    
    @staticmethod
    @transaction.atomic
    def fail_withdrawal(withdrawal_id, reason=''):
        """
        Fail withdrawal and reverse the debit transaction
        """
        try:
            withdrawal = Withdrawal.objects.select_for_update().get(id=withdrawal_id)
        except Withdrawal.DoesNotExist:
            raise TransactionError('Withdrawal not found')
        
        if withdrawal.status not in ['pending', 'processing']:
            raise TransactionError(f'Cannot fail withdrawal in status: {withdrawal.status}')
        
        # Reverse the debit transaction
        txn_reference = f'WD-{withdrawal.reference}'
        try:
            TransactionService.reverse_transaction(txn_reference, reason=reason)
        except Exception as e:
            logger.error(f"Failed to reverse withdrawal transaction: {str(e)}")
        
        withdrawal.status = 'failed'
        withdrawal.notes = reason
        withdrawal.save(update_fields=['status', 'notes', 'updated_at'])
        
        logger.info(f"Failed withdrawal: {withdrawal.reference}, reason: {reason}")
        return withdrawal
