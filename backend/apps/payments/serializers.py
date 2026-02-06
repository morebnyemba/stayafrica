from rest_framework import serializers
from apps.payments.models import Payment, Wallet, WalletTransaction, Withdrawal, BankAccount, PaymentMethod
from django.contrib.auth import get_user_model

User = get_user_model()


class PaymentMethodSerializer(serializers.ModelSerializer):
    """Serializer for PaymentMethod model"""
    
    class Meta:
        model = PaymentMethod
        fields = [
            'id', 'provider', 'method_type', 'name', 'last_four',
            'expiry_month', 'expiry_year', 'phone_number',
            'is_default', 'is_verified', 'created_at', 'updated_at'
        ]
        read_only_fields = ['id', 'created_at', 'updated_at', 'is_verified']
        extra_kwargs = {
            'phone_number': {'write_only': True}
        }
    
    def validate(self, data):
        """Validate payment method data"""
        method_type = data.get('method_type')
        
        # Card validation
        if method_type == 'card':
            if not data.get('last_four'):
                raise serializers.ValidationError({'last_four': 'Last four digits required for card'})
        
        # Mobile money validation
        elif method_type == 'mobile':
            if not data.get('phone_number'):
                raise serializers.ValidationError({'phone_number': 'Phone number required for mobile money'})
        
        return data
    
    def create(self, validated_data):
        """Create payment method"""
        # Set user from request context
        validated_data['user'] = self.context['request'].user
        return super().create(validated_data)


class PaymentMethodCreateSerializer(serializers.Serializer):
    """Serializer for creating payment methods with tokenization"""
    provider = serializers.ChoiceField(choices=PaymentMethod.PROVIDER_CHOICES)
    method_type = serializers.ChoiceField(choices=PaymentMethod.METHOD_TYPE_CHOICES)
    name = serializers.CharField(max_length=100)
    token = serializers.CharField(max_length=500, write_only=True, help_text='Token from payment provider')
    
    # Optional fields based on method type
    last_four = serializers.CharField(max_length=4, required=False)
    expiry_month = serializers.IntegerField(required=False, min_value=1, max_value=12)
    expiry_year = serializers.IntegerField(required=False, min_value=2024)
    phone_number = serializers.CharField(max_length=20, required=False)
    is_default = serializers.BooleanField(default=False)


class PaymentSerializer(serializers.ModelSerializer):
    booking_ref = serializers.CharField(source='booking.booking_ref', read_only=True)
    
    class Meta:
        model = Payment
        fields = [
            'id', 'booking', 'booking_ref', 'provider', 'gateway_ref', 'status',
            'amount', 'currency', 'created_at', 'updated_at'
        ]
        read_only_fields = ['id', 'gateway_ref', 'created_at', 'updated_at']


class WalletSerializer(serializers.ModelSerializer):
    user_email = serializers.CharField(source='user.email', read_only=True)
    user_name = serializers.SerializerMethodField()
    
    class Meta:
        model = Wallet
        fields = [
            'id', 'user', 'user_email', 'user_name', 'balance', 
            'currency', 'status', 'created_at', 'updated_at'
        ]
        read_only_fields = ['id', 'balance', 'created_at', 'updated_at']
    
    def get_user_name(self, obj):
        return f"{obj.user.first_name} {obj.user.last_name}".strip() or obj.user.email


class WalletTransactionSerializer(serializers.ModelSerializer):
    wallet_owner = serializers.CharField(source='wallet.user.email', read_only=True)
    booking_ref = serializers.CharField(source='booking.booking_ref', read_only=True, allow_null=True)
    
    class Meta:
        model = WalletTransaction
        fields = [
            'id', 'wallet', 'wallet_owner', 'booking', 'booking_ref',
            'txn_type', 'status', 'amount', 'currency', 'reference',
            'metadata', 'created_at', 'updated_at'
        ]
        read_only_fields = ['id', 'reference', 'created_at', 'updated_at']


class WalletTransactionCreateSerializer(serializers.ModelSerializer):
    class Meta:
        model = WalletTransaction
        fields = ['wallet', 'booking', 'txn_type', 'amount', 'currency', 'metadata']
    
    def validate(self, data):
        wallet = data.get('wallet')
        txn_type = data.get('txn_type')
        amount = data.get('amount')
        
        # Validate wallet is active
        if wallet.status != 'active':
            raise serializers.ValidationError('Wallet is not active')
        
        # Validate sufficient balance for debit transactions
        if txn_type == 'debit' and wallet.balance < amount:
            raise serializers.ValidationError('Insufficient wallet balance')
        
        return data


class BankAccountSerializer(serializers.ModelSerializer):
    class Meta:
        model = BankAccount
        fields = [
            'id', 'user', 'bank_name', 'account_name', 'account_number',
            'branch_code', 'country', 'is_primary', 'created_at', 'updated_at'
        ]
        read_only_fields = ['id', 'created_at', 'updated_at']
    
    def validate(self, data):
        # Ensure only one primary account per user
        user = data.get('user', self.instance.user if self.instance else None)
        is_primary = data.get('is_primary', False)
        
        if is_primary and user:
            existing_primary = BankAccount.objects.filter(
                user=user, is_primary=True
            ).exclude(id=self.instance.id if self.instance else None).exists()
            
            if existing_primary:
                raise serializers.ValidationError(
                    'User already has a primary bank account. Set is_primary=False for existing account first.'
                )
        
        return data


class WithdrawalSerializer(serializers.ModelSerializer):
    wallet_owner = serializers.CharField(source='wallet.user.email', read_only=True)
    bank_account_details = BankAccountSerializer(source='bank_account', read_only=True)
    
    class Meta:
        model = Withdrawal
        fields = [
            'id', 'wallet', 'wallet_owner', 'bank_account', 'bank_account_details',
            'amount', 'currency', 'status', 'reference', 'processed_at',
            'notes', 'created_at', 'updated_at'
        ]
        read_only_fields = ['id', 'reference', 'status', 'processed_at', 'created_at', 'updated_at']


class WithdrawalCreateSerializer(serializers.ModelSerializer):
    class Meta:
        model = Withdrawal
        fields = ['wallet', 'bank_account', 'amount', 'currency', 'notes']
    
    def validate(self, data):
        wallet = data.get('wallet')
        bank_account = data.get('bank_account')
        amount = data.get('amount')
        
        # Validate wallet is active
        if wallet.status != 'active':
            raise serializers.ValidationError('Wallet is not active')
        
        # Validate bank account belongs to wallet owner
        if bank_account.user != wallet.user:
            raise serializers.ValidationError('Bank account does not belong to wallet owner')
        
        # Validate sufficient balance
        if wallet.balance < amount:
            raise serializers.ValidationError({
                'amount': f'Insufficient balance. Available: {wallet.balance} {wallet.currency}'
            })
        
        # Validate minimum withdrawal amount
        min_withdrawal = 10  # Can be made configurable
        if amount < min_withdrawal:
            raise serializers.ValidationError({
                'amount': f'Minimum withdrawal amount is {min_withdrawal} {wallet.currency}'
            })
        
        return data
