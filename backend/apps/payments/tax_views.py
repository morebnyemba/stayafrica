"""
Views for Tax Collection System
"""
from rest_framework import viewsets, status
from rest_framework.decorators import action
from rest_framework.response import Response
from rest_framework.permissions import IsAuthenticated, IsAdminUser
from django.utils import timezone
from datetime import timedelta
from apps.payments.tax_models import (
    TaxJurisdiction,
    TaxRate,
    BookingTax,
    TaxRemittance,
    TaxExemption
)
from apps.payments.tax_serializers import (
    TaxJurisdictionSerializer,
    TaxRateSerializer,
    BookingTaxSerializer,
    TaxRemittanceSerializer,
    TaxExemptionSerializer,
    TaxReportSerializer,
    TaxSummarySerializer
)
from services.tax_collection_service import TaxCollectionService
import logging

logger = logging.getLogger(__name__)


class TaxJurisdictionViewSet(viewsets.ModelViewSet):
    """ViewSet for tax jurisdictions (admin only)"""
    serializer_class = TaxJurisdictionSerializer
    permission_classes = [IsAdminUser]
    queryset = TaxJurisdiction.objects.all()
    
    def get_queryset(self):
        queryset = TaxJurisdiction.objects.all()
        
        # Filter by country
        country = self.request.query_params.get('country')
        if country:
            queryset = queryset.filter(country_code=country)
        
        # Filter by type
        jurisdiction_type = self.request.query_params.get('type')
        if jurisdiction_type:
            queryset = queryset.filter(jurisdiction_type=jurisdiction_type)
        
        return queryset.order_by('country_code', 'name')


class TaxRateViewSet(viewsets.ModelViewSet):
    """ViewSet for tax rates (admin only)"""
    serializer_class = TaxRateSerializer
    permission_classes = [IsAdminUser]
    queryset = TaxRate.objects.all()
    
    def get_queryset(self):
        queryset = TaxRate.objects.all()
        
        # Filter by jurisdiction
        jurisdiction_id = self.request.query_params.get('jurisdiction')
        if jurisdiction_id:
            queryset = queryset.filter(jurisdiction_id=jurisdiction_id)
        
        # Filter by active status
        is_active = self.request.query_params.get('is_active')
        if is_active is not None:
            queryset = queryset.filter(is_active=is_active.lower() == 'true')
        
        return queryset.order_by('-effective_from')


class BookingTaxViewSet(viewsets.ReadOnlyModelViewSet):
    """ViewSet for booking taxes"""
    serializer_class = BookingTaxSerializer
    permission_classes = [IsAuthenticated]
    
    def get_queryset(self):
        user = self.request.user
        
        # Hosts can see taxes for their properties
        # Guests can see taxes for their bookings
        queryset = BookingTax.objects.filter(
            models.Q(booking__guest=user) |
            models.Q(booking__rental_property__host=user)
        )
        
        # Filter by booking
        booking_id = self.request.query_params.get('booking')
        if booking_id:
            queryset = queryset.filter(booking_id=booking_id)
        
        return queryset.select_related('booking', 'tax_rate')


class TaxRemittanceViewSet(viewsets.ModelViewSet):
    """ViewSet for tax remittances (admin only)"""
    serializer_class = TaxRemittanceSerializer
    permission_classes = [IsAdminUser]
    queryset = TaxRemittance.objects.all()
    
    def get_queryset(self):
        queryset = TaxRemittance.objects.all()
        
        # Filter by jurisdiction
        jurisdiction_id = self.request.query_params.get('jurisdiction')
        if jurisdiction_id:
            queryset = queryset.filter(jurisdiction_id=jurisdiction_id)
        
        # Filter by status
        status_filter = self.request.query_params.get('status')
        if status_filter:
            queryset = queryset.filter(status=status_filter)
        
        return queryset.order_by('-period_end')
    
    @action(detail=True, methods=['post'])
    def process(self, request, pk=None):
        """Process (complete) a remittance"""
        remittance = self.get_object()
        
        if remittance.status == 'completed':
            return Response(
                {'error': 'Remittance already completed'},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        reference_number = request.data.get('reference_number')
        
        processed_remittance = TaxCollectionService.process_remittance(
            remittance.id,
            reference_number
        )
        
        serializer = TaxRemittanceSerializer(processed_remittance)
        return Response(serializer.data)
    
    @action(detail=False, methods=['post'])
    def create_for_period(self, request):
        """Create remittance for a jurisdiction and period"""
        jurisdiction_id = request.data.get('jurisdiction_id')
        start_date = request.data.get('start_date')
        end_date = request.data.get('end_date')
        
        if not all([jurisdiction_id, start_date, end_date]):
            return Response(
                {'error': 'jurisdiction_id, start_date, and end_date required'},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        try:
            jurisdiction = TaxJurisdiction.objects.get(id=jurisdiction_id)
        except TaxJurisdiction.DoesNotExist:
            return Response(
                {'error': 'Jurisdiction not found'},
                status=status.HTTP_404_NOT_FOUND
            )
        
        remittance = TaxCollectionService.create_remittance(
            jurisdiction,
            start_date,
            end_date
        )
        
        serializer = TaxRemittanceSerializer(remittance)
        return Response(serializer.data, status=status.HTTP_201_CREATED)


class TaxReportViewSet(viewsets.ViewSet):
    """ViewSet for tax reports"""
    permission_classes = [IsAuthenticated]
    
    @action(detail=False, methods=['get'])
    def host_summary(self, request):
        """Get tax summary for logged-in host"""
        host = request.user
        
        # Get date range from query params
        start_date = request.query_params.get('start_date')
        end_date = request.query_params.get('end_date')
        
        if start_date:
            from datetime import date
            start_date = date.fromisoformat(start_date)
        if end_date:
            from datetime import date
            end_date = date.fromisoformat(end_date)
        
        summary = TaxCollectionService.get_tax_summary_for_host(
            host, start_date, end_date
        )
        
        serializer = TaxSummarySerializer(summary)
        return Response(serializer.data)
    
    @action(detail=False, methods=['get'], permission_classes=[IsAdminUser])
    def jurisdiction_report(self, request):
        """Generate tax report for jurisdiction (admin only)"""
        jurisdiction_id = request.query_params.get('jurisdiction_id')
        start_date = request.query_params.get('start_date')
        end_date = request.query_params.get('end_date')
        
        if not all([jurisdiction_id, start_date, end_date]):
            return Response(
                {'error': 'jurisdiction_id, start_date, and end_date required'},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        try:
            jurisdiction = TaxJurisdiction.objects.get(id=jurisdiction_id)
        except TaxJurisdiction.DoesNotExist:
            return Response(
                {'error': 'Jurisdiction not found'},
                status=status.HTTP_404_NOT_FOUND
            )
        
        from datetime import date
        start_date = date.fromisoformat(start_date)
        end_date = date.fromisoformat(end_date)
        
        report = TaxCollectionService.generate_tax_report(
            jurisdiction, start_date, end_date
        )
        
        serializer = TaxReportSerializer(report)
        return Response(serializer.data)


class TaxExemptionViewSet(viewsets.ModelViewSet):
    """ViewSet for tax exemptions"""
    serializer_class = TaxExemptionSerializer
    permission_classes = [IsAdminUser]
    queryset = TaxExemption.objects.all()
    
    def get_queryset(self):
        queryset = TaxExemption.objects.all()
        
        # Filter by property
        property_id = self.request.query_params.get('property')
        if property_id:
            queryset = queryset.filter(property_id=property_id)
        
        # Filter by host
        host_id = self.request.query_params.get('host')
        if host_id:
            queryset = queryset.filter(host_id=host_id)
        
        # Filter by active status
        is_active = self.request.query_params.get('is_active')
        if is_active is not None:
            queryset = queryset.filter(is_active=is_active.lower() == 'true')
        
        return queryset.order_by('-valid_from')
