from django.urls import path, include
from rest_framework.routers import DefaultRouter
from apps.payments.views import (
    PaymentViewSet, WalletViewSet, WalletTransactionViewSet, 
    BankAccountViewSet, WithdrawalViewSet
)
from apps.payments.tax_views import (
    TaxJurisdictionViewSet,
    TaxRateViewSet,
    BookingTaxViewSet,
    TaxRemittanceViewSet,
    TaxReportViewSet,
    TaxExemptionViewSet
)

app_name = 'payments'

router = DefaultRouter()
router.register(r'payments', PaymentViewSet, basename='payment')
router.register(r'wallets', WalletViewSet, basename='wallet')
router.register(r'transactions', WalletTransactionViewSet, basename='transaction')
router.register(r'bank-accounts', BankAccountViewSet, basename='bank-account')
router.register(r'withdrawals', WithdrawalViewSet, basename='withdrawal')
router.register(r'tax/jurisdictions', TaxJurisdictionViewSet, basename='tax-jurisdiction')
router.register(r'tax/rates', TaxRateViewSet, basename='tax-rate')
router.register(r'tax/booking-taxes', BookingTaxViewSet, basename='booking-tax')
router.register(r'tax/remittances', TaxRemittanceViewSet, basename='tax-remittance')
router.register(r'tax/reports', TaxReportViewSet, basename='tax-report')
router.register(r'tax/exemptions', TaxExemptionViewSet, basename='tax-exemption')

urlpatterns = [
    path('', include(router.urls)),
]
