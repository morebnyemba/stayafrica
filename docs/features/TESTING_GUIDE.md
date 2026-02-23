# Testing Guide - Transaction Management and Messaging

This guide provides comprehensive testing procedures for the new transaction management and Erlang messaging implementations.

## 1. Transaction Service Testing

### Unit Tests

Create `backend/tests/test_transaction_service.py`:

```python
import pytest
from decimal import Decimal
from django.contrib.auth import get_user_model
from services.transaction_service import (
    TransactionService, TransactionError, InsufficientBalanceError
)
from apps.payments.models import Wallet, WalletTransaction, BankAccount
from apps.bookings.models import Booking

User = get_user_model()


@pytest.fixture
def user(db):
    return User.objects.create_user(
        email='test@example.com',
        password='testpass123'
    )


@pytest.fixture
def wallet(user):
    return TransactionService.create_wallet(user)


@pytest.fixture
def bank_account(user):
    return BankAccount.objects.create(
        user=user,
        bank_name='Test Bank',
        account_name='Test Account',
        account_number='1234567890',
        is_primary=True
    )


class TestWalletCreation:
    def test_create_wallet(self, user):
        wallet = TransactionService.create_wallet(user)
        assert wallet.user == user
        assert wallet.balance == Decimal('0.00')
        assert wallet.status == 'active'
    
    def test_create_wallet_idempotent(self, user):
        wallet1 = TransactionService.create_wallet(user)
        wallet2 = TransactionService.create_wallet(user)
        assert wallet1.id == wallet2.id


class TestWalletOperations:
    def test_credit_wallet(self, wallet):
        txn = TransactionService.credit_wallet(
            wallet=wallet,
            amount=Decimal('100.00')
        )
        
        wallet.refresh_from_db()
        assert wallet.balance == Decimal('100.00')
        assert txn.status == 'completed'
        assert txn.txn_type == 'credit'
        assert txn.amount == Decimal('100.00')
    
    def test_debit_wallet(self, wallet):
        # First credit
        TransactionService.credit_wallet(wallet, Decimal('100.00'))
        
        # Then debit
        txn = TransactionService.debit_wallet(
            wallet=wallet,
            amount=Decimal('50.00')
        )
        
        wallet.refresh_from_db()
        assert wallet.balance == Decimal('50.00')
        assert txn.status == 'completed'
        assert txn.txn_type == 'debit'
    
    def test_debit_insufficient_balance(self, wallet):
        with pytest.raises(InsufficientBalanceError):
            TransactionService.debit_wallet(
                wallet=wallet,
                amount=Decimal('100.00')
            )
    
    def test_credit_negative_amount(self, wallet):
        with pytest.raises(TransactionError):
            TransactionService.credit_wallet(
                wallet=wallet,
                amount=Decimal('-10.00')
            )
    
    def test_inactive_wallet(self, wallet):
        wallet.status = 'suspended'
        wallet.save()
        
        with pytest.raises(TransactionError):
            TransactionService.credit_wallet(
                wallet=wallet,
                amount=Decimal('100.00')
            )


class TestTransactionReversal:
    def test_reverse_credit(self, wallet):
        # Credit wallet
        credit_txn = TransactionService.credit_wallet(
            wallet=wallet,
            amount=Decimal('100.00')
        )
        
        # Reverse
        reversal_txn = TransactionService.reverse_transaction(
            txn_reference=credit_txn.reference,
            reason='Test reversal'
        )
        
        wallet.refresh_from_db()
        credit_txn.refresh_from_db()
        
        assert wallet.balance == Decimal('0.00')
        assert credit_txn.status == 'reversed'
        assert reversal_txn.status == 'completed'
        assert reversal_txn.txn_type == 'debit'
    
    def test_reverse_debit(self, wallet):
        # Credit then debit
        TransactionService.credit_wallet(wallet, Decimal('100.00'))
        debit_txn = TransactionService.debit_wallet(
            wallet=wallet,
            amount=Decimal('50.00')
        )
        
        # Reverse debit
        reversal_txn = TransactionService.reverse_transaction(
            txn_reference=debit_txn.reference,
            reason='Refund'
        )
        
        wallet.refresh_from_db()
        assert wallet.balance == Decimal('100.00')
        assert reversal_txn.txn_type == 'credit'
    
    def test_reverse_nonexistent(self):
        with pytest.raises(TransactionError):
            TransactionService.reverse_transaction('INVALID-REF')


class TestWithdrawals:
    def test_initiate_withdrawal(self, wallet, bank_account):
        # Credit wallet first
        TransactionService.credit_wallet(wallet, Decimal('500.00'))
        
        # Initiate withdrawal
        withdrawal = TransactionService.initiate_withdrawal(
            wallet=wallet,
            bank_account=bank_account,
            amount=Decimal('200.00')
        )
        
        wallet.refresh_from_db()
        assert wallet.balance == Decimal('300.00')  # Locked
        assert withdrawal.status == 'pending'
        assert withdrawal.amount == Decimal('200.00')
    
    def test_withdrawal_insufficient_balance(self, wallet, bank_account):
        with pytest.raises(InsufficientBalanceError):
            TransactionService.initiate_withdrawal(
                wallet=wallet,
                bank_account=bank_account,
                amount=Decimal('100.00')
            )
    
    def test_complete_withdrawal(self, wallet, bank_account):
        # Setup
        TransactionService.credit_wallet(wallet, Decimal('500.00'))
        withdrawal = TransactionService.initiate_withdrawal(
            wallet=wallet,
            bank_account=bank_account,
            amount=Decimal('200.00')
        )
        
        # Complete
        completed = TransactionService.complete_withdrawal(
            withdrawal_id=withdrawal.id,
            notes='Processed successfully'
        )
        
        assert completed.status == 'completed'
        assert completed.processed_at is not None
        assert wallet.balance == Decimal('300.00')
    
    def test_fail_withdrawal(self, wallet, bank_account):
        # Setup
        TransactionService.credit_wallet(wallet, Decimal('500.00'))
        withdrawal = TransactionService.initiate_withdrawal(
            wallet=wallet,
            bank_account=bank_account,
            amount=Decimal('200.00')
        )
        
        # Fail
        failed = TransactionService.fail_withdrawal(
            withdrawal_id=withdrawal.id,
            reason='Bank account invalid'
        )
        
        wallet.refresh_from_db()
        assert failed.status == 'failed'
        assert wallet.balance == Decimal('500.00')  # Refunded
```

### Integration Tests

Create `backend/tests/test_payment_integration.py`:

```python
import pytest
from decimal import Decimal
from django.urls import reverse
from rest_framework.test import APIClient
from rest_framework import status
from services.transaction_service import TransactionService
from apps.bookings.models import Booking
from apps.payments.models import Payment, BankAccount


@pytest.fixture
def api_client():
    return APIClient()


@pytest.fixture
def authenticated_client(api_client, user):
    api_client.force_authenticate(user=user)
    return api_client


class TestWalletAPI:
    def test_get_my_wallet(self, authenticated_client, user):
        # Create wallet
        TransactionService.create_wallet(user)
        
        # Get wallet
        url = reverse('payments:wallet-my-wallet')
        response = authenticated_client.get(url)
        
        assert response.status_code == status.HTTP_200_OK
        assert response.data['balance'] == '0.00'
        assert response.data['status'] == 'active'
    
    def test_get_wallet_balance(self, authenticated_client, user):
        wallet = TransactionService.create_wallet(user)
        TransactionService.credit_wallet(wallet, Decimal('100.00'))
        
        url = reverse('payments:wallet-balance', kwargs={'pk': wallet.id})
        response = authenticated_client.get(url)
        
        assert response.status_code == status.HTTP_200_OK
        assert Decimal(response.data['balance']) == Decimal('100.00')
    
    def test_get_transactions(self, authenticated_client, user):
        wallet = TransactionService.create_wallet(user)
        TransactionService.credit_wallet(wallet, Decimal('100.00'))
        TransactionService.debit_wallet(wallet, Decimal('50.00'))
        
        url = reverse('payments:wallet-transactions', kwargs={'pk': wallet.id})
        response = authenticated_client.get(url)
        
        assert response.status_code == status.HTTP_200_OK
        assert response.data['count'] == 2


class TestBankAccountAPI:
    def test_create_bank_account(self, authenticated_client):
        url = reverse('payments:bank-account-list')
        data = {
            'bank_name': 'Test Bank',
            'account_name': 'Test Account',
            'account_number': '1234567890',
            'branch_code': '001',
            'country': 'Zimbabwe',
            'is_primary': True
        }
        
        response = authenticated_client.post(url, data)
        
        assert response.status_code == status.HTTP_201_CREATED
        assert response.data['bank_name'] == 'Test Bank'
        assert response.data['is_primary'] is True
    
    def test_set_primary_account(self, authenticated_client, user):
        # Create two accounts
        account1 = BankAccount.objects.create(
            user=user,
            bank_name='Bank 1',
            account_name='Account 1',
            account_number='111',
            is_primary=True
        )
        account2 = BankAccount.objects.create(
            user=user,
            bank_name='Bank 2',
            account_name='Account 2',
            account_number='222',
            is_primary=False
        )
        
        # Set account2 as primary
        url = reverse('payments:bank-account-set-primary', kwargs={'pk': account2.id})
        response = authenticated_client.post(url)
        
        assert response.status_code == status.HTTP_200_OK
        
        # Check primary status
        account1.refresh_from_db()
        account2.refresh_from_db()
        assert not account1.is_primary
        assert account2.is_primary


class TestWithdrawalAPI:
    def test_initiate_withdrawal(self, authenticated_client, user):
        # Setup
        wallet = TransactionService.create_wallet(user)
        TransactionService.credit_wallet(wallet, Decimal('500.00'))
        bank_account = BankAccount.objects.create(
            user=user,
            bank_name='Test Bank',
            account_name='Test',
            account_number='123',
            is_primary=True
        )
        
        # Initiate withdrawal
        url = reverse('payments:withdrawal-list')
        data = {
            'wallet': wallet.id,
            'bank_account': bank_account.id,
            'amount': '200.00',
            'currency': 'USD'
        }
        
        response = authenticated_client.post(url, data)
        
        assert response.status_code == status.HTTP_201_CREATED
        assert response.data['status'] == 'pending'
        assert Decimal(response.data['amount']) == Decimal('200.00')
    
    def test_withdrawal_insufficient_balance(self, authenticated_client, user):
        wallet = TransactionService.create_wallet(user)
        bank_account = BankAccount.objects.create(
            user=user,
            bank_name='Test Bank',
            account_name='Test',
            account_number='123',
            is_primary=True
        )
        
        url = reverse('payments:withdrawal-list')
        data = {
            'wallet': wallet.id,
            'bank_account': bank_account.id,
            'amount': '200.00',
            'currency': 'USD'
        }
        
        response = authenticated_client.post(url, data)
        
        assert response.status_code == status.HTTP_400_BAD_REQUEST
```

## 2. Erlang Messaging Testing

### Manual Testing

```bash
# 1. Start Erlang service
cd erlang_messaging
rebar3 shell

# 2. Test health endpoint
curl http://localhost:8765/health

# Expected output:
# {"status":"ok","service":"messaging_service","timestamp":1234567890}

# 3. Send a message
curl -X POST http://localhost:8765/api/messages/send \
  -H "Content-Type: application/json" \
  -d '{
    "conversation_id": 1,
    "sender_id": 1,
    "receiver_id": 2,
    "text": "Hello from Erlang!",
    "message_type": "text",
    "priority": "normal"
  }'

# Expected output:
# {"status":"success","message":"Message queued"}

# 4. Get user messages
curl http://localhost:8765/api/messages/queue/2

# Expected output:
# {"status":"success","user_id":"2","messages":[...],"count":1}

# 5. Broadcast message
curl -X POST http://localhost:8765/api/messages/broadcast \
  -H "Content-Type: application/json" \
  -d '{
    "message": {
      "sender_id": 1,
      "text": "System announcement",
      "message_type": "system"
    },
    "user_ids": [2, 3, 4]
  }'

# 6. Get stats
curl http://localhost:8765/api/stats

# Expected output:
# {"counters":{...},"uptime_seconds":123,"memory":{...},"process_count":45}
```

### Django Integration Testing

```python
# backend/tests/test_erlang_integration.py

import pytest
from services.erlang_messaging import erlang_client
from apps.messaging.models import Message, Conversation
from django.contrib.auth import get_user_model

User = get_user_model()


@pytest.fixture
def users(db):
    user1 = User.objects.create_user(email='user1@test.com', password='pass')
    user2 = User.objects.create_user(email='user2@test.com', password='pass')
    return user1, user2


@pytest.fixture
def conversation(users):
    conv = Conversation.objects.create(subject='Test')
    conv.participants.set(users)
    return conv


class TestErlangClient:
    def test_health_check(self):
        # May fail if Erlang service not running
        is_healthy = erlang_client.health_check()
        assert isinstance(is_healthy, bool)
    
    @pytest.mark.skipif(
        not erlang_client.health_check(),
        reason="Erlang service not available"
    )
    def test_send_message(self, conversation, users):
        sender, receiver = users
        
        success = erlang_client.send_message(
            conversation_id=conversation.id,
            sender_id=sender.id,
            receiver_id=receiver.id,
            text="Test message",
            message_type='text',
            priority='normal'
        )
        
        assert success is True
    
    @pytest.mark.skipif(
        not erlang_client.health_check(),
        reason="Erlang service not available"
    )
    def test_broadcast_message(self, users):
        sender, receiver = users
        
        success = erlang_client.broadcast_message(
            sender_id=sender.id,
            user_ids=[receiver.id],
            text="Broadcast test",
            message_type='system'
        )
        
        assert success is True
    
    @pytest.mark.skipif(
        not erlang_client.health_check(),
        reason="Erlang service not available"
    )
    def test_get_stats(self):
        stats = erlang_client.get_stats()
        
        if stats:  # May be None if service not available
            assert 'counters' in stats
            assert 'uptime_seconds' in stats
```

## 3. Load Testing

### Transaction Service Load Test

```python
# backend/tests/test_transaction_load.py

import pytest
from decimal import Decimal
from concurrent.futures import ThreadPoolExecutor, as_completed
from services.transaction_service import TransactionService


def credit_wallet(wallet, amount):
    """Helper function for concurrent credits"""
    return TransactionService.credit_wallet(wallet, amount)


@pytest.mark.slow
def test_concurrent_credits(user):
    """Test concurrent wallet credits"""
    wallet = TransactionService.create_wallet(user)
    
    # Perform 100 concurrent credits
    with ThreadPoolExecutor(max_workers=10) as executor:
        futures = [
            executor.submit(credit_wallet, wallet, Decimal('1.00'))
            for _ in range(100)
        ]
        
        # Wait for all to complete
        for future in as_completed(futures):
            txn = future.result()
            assert txn.status == 'completed'
    
    # Verify final balance
    wallet.refresh_from_db()
    assert wallet.balance == Decimal('100.00')
```

### Erlang Load Test

```bash
# Use Apache Bench or similar tool
ab -n 10000 -c 100 -p message.json -T application/json \
  http://localhost:8765/api/messages/send

# Expected: 10000+ requests/second
```

## 4. End-to-End Testing

### Complete Booking Flow

```python
def test_complete_booking_flow(authenticated_client, user, property_obj):
    """Test complete flow: booking → payment → host payout"""
    
    # 1. Create booking
    booking_data = {
        'rental_property': property_obj.id,
        'check_in': '2024-02-01',
        'check_out': '2024-02-05',
        'cleaning_fee': '0.00'
    }
    response = authenticated_client.post(
        reverse('bookings:booking-list'),
        booking_data
    )
    booking_id = response.data['id']
    
    # 2. Initiate payment
    payment_data = {
        'booking_id': booking_id,
        'provider': 'stripe'
    }
    response = authenticated_client.post(
        reverse('payments:payment-initiate'),
        payment_data
    )
    payment_ref = response.data['gateway_ref']
    
    # 3. Simulate successful webhook
    webhook_data = {
        'provider': 'stripe',
        'gateway_ref': payment_ref,
        'status': 'success'
    }
    response = authenticated_client.post(
        reverse('payments:payment-webhook'),
        webhook_data
    )
    
    # 4. Verify host wallet credited
    host = property_obj.host
    wallet = Wallet.objects.get(user=host)
    assert wallet.balance > 0
    
    # 5. Verify booking confirmed
    booking = Booking.objects.get(id=booking_id)
    assert booking.status == 'confirmed'
```

## 5. Monitoring Tests

```python
def test_erlang_monitoring():
    """Test that Erlang service is being monitored"""
    from django.urls import reverse
    
    url = reverse('messaging:erlang-health')
    response = client.get(url)
    
    # Should return 200 if healthy or 503 if unhealthy
    assert response.status_code in [200, 503]
    
    if response.status_code == 200:
        assert 'erlang_stats' in response.json()
```

## Running Tests

```bash
# Run all tests
pytest backend/tests/

# Run specific test file
pytest backend/tests/test_transaction_service.py -v

# Run with coverage
pytest backend/tests/ --cov=backend/services --cov-report=html

# Run only fast tests (exclude slow/load tests)
pytest backend/tests/ -m "not slow"

# Run only integration tests
pytest backend/tests/ -m integration
```

## Test Coverage Goals

- **Transaction Service**: > 90% coverage
- **Erlang Client**: > 85% coverage
- **API Endpoints**: > 80% coverage
- **Integration Tests**: All critical paths covered

## Continuous Integration

Add to `.github/workflows/test.yml`:

```yaml
name: Tests

on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest
    
    services:
      postgres:
        image: postgres:14
        env:
          POSTGRES_PASSWORD: postgres
        options: >-
          --health-cmd pg_isready
          --health-interval 10s
          --health-timeout 5s
          --health-retries 5
      
      redis:
        image: redis:7
        options: >-
          --health-cmd "redis-cli ping"
          --health-interval 10s
          --health-timeout 5s
          --health-retries 5
    
    steps:
      - uses: actions/checkout@v2
      
      - name: Set up Python
        uses: actions/setup-python@v2
        with:
          python-version: '3.11'
      
      - name: Install dependencies
        run: |
          pip install -r backend/requirements.txt
          pip install -r backend/requirements-dev.txt
      
      - name: Run tests
        env:
          DATABASE_URL: postgres://postgres:postgres@localhost/test_db
          REDIS_URL: redis://localhost:6379/0
        run: |
          cd backend
          pytest --cov=. --cov-report=xml
      
      - name: Upload coverage
        uses: codecov/codecov-action@v2
```

## Performance Benchmarks

### Target Metrics

**Transaction Service:**
- Wallet operations: < 50ms p99
- Concurrent operations: 1000+ ops/second
- Transaction reversals: < 100ms

**Erlang Messaging:**
- Message throughput: > 50,000 msg/s
- Latency: < 1ms p50, < 5ms p99
- Concurrent connections: > 10,000
- Memory: < 500MB for 10,000 connections

## Troubleshooting Tests

### Common Issues

1. **Erlang service not available**
   - Skip Erlang tests: `pytest -m "not erlang"`
   - Start Erlang service before testing

2. **Database constraints**
   - Use `@pytest.mark.django_db` decorator
   - Clear database between tests: `pytest --reuse-db`

3. **Async timing issues**
   - Add appropriate wait times
   - Use `time.sleep()` or `await` as needed

4. **Transaction rollback in tests**
   - Django test runner wraps in transaction
   - Use `transaction_on_commit` for real commits
