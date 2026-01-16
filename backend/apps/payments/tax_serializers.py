"""
Serializers for Tax Collection System
"""
from rest_framework import serializers
from apps.payments.tax_models import (
    TaxJurisdiction,
    TaxRate,
    BookingTax,
    TaxRemittance,
    TaxExemption
)


class TaxJurisdictionSerializer(serializers.ModelSerializer):
    """Serializer for tax jurisdictions"""
    jurisdiction_type_display = serializers.CharField(
        source='get_jurisdiction_type_display',
        read_only=True
    )
    
    class Meta:
        model = TaxJurisdiction
        fields = [
            'id', 'name', 'jurisdiction_type', 'jurisdiction_type_display',
            'code', 'country_code', 'state_province_code', 'city_name',
            'parent_jurisdiction', 'is_active', 'created_at'
        ]
        read_only_fields = ['id', 'created_at']


class TaxRateSerializer(serializers.ModelSerializer):
    """Serializer for tax rates"""
    jurisdiction_name = serializers.CharField(source='jurisdiction.name', read_only=True)
    tax_type_display = serializers.CharField(source='get_tax_type_display', read_only=True)
    
    class Meta:
        model = TaxRate
        fields = [
            'id', 'jurisdiction', 'jurisdiction_name', 'name',
            'tax_type', 'tax_type_display', 'rate_percentage',
            'flat_fee', 'applies_to_base_price', 'applies_to_cleaning_fee',
            'applies_to_service_fee', 'effective_from', 'effective_to',
            'collected_by_platform', 'is_active', 'created_at'
        ]
        read_only_fields = ['id', 'created_at']


class BookingTaxSerializer(serializers.ModelSerializer):
    """Serializer for booking taxes"""
    tax_name = serializers.CharField(source='tax_rate.name', read_only=True)
    tax_rate_percentage = serializers.DecimalField(
        source='tax_rate.rate_percentage',
        max_digits=5,
        decimal_places=2,
        read_only=True
    )
    booking_ref = serializers.CharField(source='booking.booking_ref', read_only=True)
    
    class Meta:
        model = BookingTax
        fields = [
            'id', 'booking', 'booking_ref', 'tax_rate', 'tax_name',
            'tax_rate_percentage', 'taxable_amount', 'tax_amount',
            'calculation_date'
        ]
        read_only_fields = fields


class TaxRemittanceSerializer(serializers.ModelSerializer):
    """Serializer for tax remittances"""
    jurisdiction_name = serializers.CharField(source='jurisdiction.name', read_only=True)
    status_display = serializers.CharField(source='get_status_display', read_only=True)
    
    class Meta:
        model = TaxRemittance
        fields = [
            'id', 'jurisdiction', 'jurisdiction_name', 'period_start',
            'period_end', 'total_tax_collected', 'total_bookings',
            'remittance_date', 'remittance_reference', 'status',
            'status_display', 'report_file_url', 'confirmation_file_url',
            'notes', 'created_at', 'updated_at'
        ]
        read_only_fields = ['id', 'created_at', 'updated_at']


class TaxExemptionSerializer(serializers.ModelSerializer):
    """Serializer for tax exemptions"""
    exemption_type_display = serializers.CharField(
        source='get_exemption_type_display',
        read_only=True
    )
    jurisdiction_name = serializers.CharField(source='jurisdiction.name', read_only=True)
    
    class Meta:
        model = TaxExemption
        fields = [
            'id', 'exemption_type', 'exemption_type_display',
            'property', 'host', 'jurisdiction', 'jurisdiction_name',
            'tax_rate', 'exemption_certificate_number', 'reason',
            'valid_from', 'valid_to', 'is_active', 'created_at'
        ]
        read_only_fields = ['id', 'created_at']


class TaxReportSerializer(serializers.Serializer):
    """Serializer for tax reports"""
    jurisdiction = serializers.DictField()
    period = serializers.DictField()
    summary = serializers.DictField()
    by_tax_type = serializers.DictField()


class TaxSummarySerializer(serializers.Serializer):
    """Serializer for tax summaries"""
    period = serializers.DictField()
    summary = serializers.DictField()
    by_jurisdiction = serializers.DictField()
