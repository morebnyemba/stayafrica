from django.urls import path, include
from rest_framework.routers import DefaultRouter
from apps.payments.views import (
    PaymentViewSet, WalletViewSet, WalletTransactionViewSet, 
    BankAccountViewSet, WithdrawalViewSet
)

app_name = 'payments'

router = DefaultRouter()
router.register(r'payments', PaymentViewSet, basename='payment')
router.register(r'wallets', WalletViewSet, basename='wallet')
router.register(r'transactions', WalletTransactionViewSet, basename='transaction')
router.register(r'bank-accounts', BankAccountViewSet, basename='bank-account')
router.register(r'withdrawals', WithdrawalViewSet, basename='withdrawal')

urlpatterns = [
    path('', include(router.urls)),
]
