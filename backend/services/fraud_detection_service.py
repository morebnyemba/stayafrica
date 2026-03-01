"""
Fraud Detection Service for StayAfrica.

Rules-based fraud scoring system that evaluates bookings and payments
for suspicious activity. Assigns risk scores and can flag or block
high-risk transactions.
"""
import logging
from decimal import Decimal
from datetime import timedelta
from django.utils import timezone
from django.db.models import Count, Sum, Q

logger = logging.getLogger(__name__)

# Risk score thresholds
RISK_LOW = 0
RISK_MEDIUM = 30
RISK_HIGH = 60
RISK_CRITICAL = 80


class FraudSignal:
    """Represents a single fraud signal with score and reason."""
    def __init__(self, score: int, reason: str, category: str = 'general'):
        self.score = score
        self.reason = reason
        self.category = category

    def to_dict(self):
        return {
            'score': self.score,
            'reason': self.reason,
            'category': self.category,
        }


class FraudDetectionService:
    """
    Evaluates bookings and payments for fraud risk.
    Returns a risk score (0-100) with detailed signals.
    """

    def assess_booking_risk(self, booking, request=None):
        """
        Evaluate fraud risk for a booking.
        Returns dict with risk_score, risk_level, signals, and action.
        """
        signals = []
        guest = booking.guest

        # Check 1: New account booking expensive property
        signals.extend(self._check_new_account_high_value(guest, booking))

        # Check 2: Rapid booking frequency
        signals.extend(self._check_booking_velocity(guest))

        # Check 3: Suspicious booking patterns
        signals.extend(self._check_booking_patterns(guest, booking))

        # Check 4: Price anomalies
        signals.extend(self._check_price_anomalies(booking))

        # Check 5: Geographic mismatch
        if request:
            signals.extend(self._check_geo_mismatch(guest, request))

        # Check 6: Failed payment history
        signals.extend(self._check_payment_history(guest))

        # Calculate total risk score (capped at 100)
        total_score = min(sum(s.score for s in signals), 100)
        risk_level = self._score_to_level(total_score)
        action = self._determine_action(total_score)

        result = {
            'risk_score': total_score,
            'risk_level': risk_level,
            'action': action,
            'signals': [s.to_dict() for s in signals],
            'assessed_at': timezone.now().isoformat(),
        }

        if total_score >= RISK_HIGH:
            logger.warning(
                f"High fraud risk detected for booking {booking.booking_ref}: "
                f"score={total_score}, level={risk_level}"
            )

        return result

    def assess_payment_risk(self, payment, request=None):
        """Evaluate fraud risk for a payment transaction."""
        signals = []
        booking = payment.booking
        guest = booking.guest

        # Check 1: Payment amount mismatch
        if payment.amount != booking.grand_total:
            signals.append(FraudSignal(
                40, 'Payment amount does not match booking total', 'payment'
            ))

        # Check 2: Multiple failed payment attempts
        signals.extend(self._check_failed_payments_for_booking(booking))

        # Check 3: Provider switching
        signals.extend(self._check_provider_switching(booking))

        # Check 4: Unusual payment timing
        signals.extend(self._check_payment_timing(booking, payment))

        # Include booking risk signals
        booking_risk = self.assess_booking_risk(booking, request)
        # Weight booking signals at 50%
        for signal_data in booking_risk['signals']:
            signals.append(FraudSignal(
                signal_data['score'] // 2,
                f"[Booking] {signal_data['reason']}",
                signal_data['category'],
            ))

        total_score = min(sum(s.score for s in signals), 100)
        risk_level = self._score_to_level(total_score)

        return {
            'risk_score': total_score,
            'risk_level': risk_level,
            'action': self._determine_action(total_score),
            'signals': [s.to_dict() for s in signals],
            'assessed_at': timezone.now().isoformat(),
        }

    # --- Individual Risk Checks ---

    def _check_new_account_high_value(self, guest, booking):
        """Flag new accounts making high-value bookings."""
        signals = []
        account_age = timezone.now() - guest.date_joined
        if account_age < timedelta(hours=24) and booking.grand_total > Decimal('500'):
            signals.append(FraudSignal(
                25, 'New account (<24h) with high-value booking (>$500)', 'account'
            ))
        elif account_age < timedelta(days=7) and booking.grand_total > Decimal('1000'):
            signals.append(FraudSignal(
                15, 'Recent account (<7d) with very high-value booking (>$1000)', 'account'
            ))
        if not guest.is_verified and booking.grand_total > Decimal('300'):
            signals.append(FraudSignal(
                10, 'Unverified account with booking >$300', 'account'
            ))
        return signals

    def _check_booking_velocity(self, guest):
        """Flag users making too many bookings in a short time."""
        signals = []
        from apps.bookings.models import Booking

        last_hour = timezone.now() - timedelta(hours=1)
        last_day = timezone.now() - timedelta(days=1)

        hourly_count = Booking.objects.filter(
            guest=guest, created_at__gte=last_hour
        ).count()
        daily_count = Booking.objects.filter(
            guest=guest, created_at__gte=last_day
        ).count()

        if hourly_count >= 5:
            signals.append(FraudSignal(
                35, f'{hourly_count} bookings in last hour', 'velocity'
            ))
        elif hourly_count >= 3:
            signals.append(FraudSignal(
                15, f'{hourly_count} bookings in last hour', 'velocity'
            ))

        if daily_count >= 10:
            signals.append(FraudSignal(
                30, f'{daily_count} bookings in last 24 hours', 'velocity'
            ))
        return signals

    def _check_booking_patterns(self, guest, booking):
        """Check for suspicious booking patterns."""
        signals = []
        from apps.bookings.models import Booking

        # Very long stays (>30 days)
        if booking.nights > 30:
            signals.append(FraudSignal(
                10, f'Unusually long stay: {booking.nights} nights', 'pattern'
            ))

        # Same-day check-in
        if booking.check_in == timezone.now().date():
            signals.append(FraudSignal(
                5, 'Same-day check-in booking', 'pattern'
            ))

        # Multiple cancelled bookings
        cancelled_count = Booking.objects.filter(
            guest=guest, status='cancelled',
            created_at__gte=timezone.now() - timedelta(days=30),
        ).count()
        if cancelled_count >= 3:
            signals.append(FraudSignal(
                20, f'{cancelled_count} cancelled bookings in last 30 days', 'pattern'
            ))

        return signals

    def _check_price_anomalies(self, booking):
        """Check for unusual pricing."""
        signals = []
        if booking.grand_total > Decimal('5000'):
            signals.append(FraudSignal(
                10, f'Very high booking value: ${booking.grand_total}', 'price'
            ))
        if booking.grand_total <= Decimal('0'):
            signals.append(FraudSignal(
                50, 'Zero or negative booking value', 'price'
            ))
        return signals

    def _check_geo_mismatch(self, guest, request):
        """Check if request origin mismatches user profile."""
        signals = []
        forwarded_for = request.META.get('HTTP_X_FORWARDED_FOR', '')
        if forwarded_for:
            # Multiple proxy hops may indicate VPN/proxy usage
            ips = [ip.strip() for ip in forwarded_for.split(',')]
            if len(ips) > 3:
                signals.append(FraudSignal(
                    10, 'Request routed through multiple proxies', 'geo'
                ))
        return signals

    def _check_payment_history(self, guest):
        """Check guest's payment failure history."""
        signals = []
        from apps.payments.models import Payment

        failed_count = Payment.objects.filter(
            booking__guest=guest,
            status='failed',
            created_at__gte=timezone.now() - timedelta(days=7),
        ).count()

        if failed_count >= 5:
            signals.append(FraudSignal(
                35, f'{failed_count} failed payments in last 7 days', 'payment'
            ))
        elif failed_count >= 2:
            signals.append(FraudSignal(
                15, f'{failed_count} failed payments in last 7 days', 'payment'
            ))
        return signals

    def _check_failed_payments_for_booking(self, booking):
        """Check failed payment attempts for this specific booking."""
        signals = []
        from apps.payments.models import Payment

        failed = Payment.objects.filter(
            booking=booking, status='failed'
        ).count()
        if failed >= 3:
            signals.append(FraudSignal(
                30, f'{failed} failed payment attempts for this booking', 'payment'
            ))
        return signals

    def _check_provider_switching(self, booking):
        """Flag rapid switching between payment providers."""
        signals = []
        from apps.payments.models import Payment

        providers_used = Payment.objects.filter(
            booking=booking
        ).values_list('provider', flat=True).distinct()

        if len(set(providers_used)) >= 3:
            signals.append(FraudSignal(
                20, 'Multiple payment providers attempted', 'payment'
            ))
        return signals

    def _check_payment_timing(self, booking, payment):
        """Check for suspicious payment timing."""
        signals = []
        time_since_booking = payment.created_at - booking.created_at
        if time_since_booking < timedelta(seconds=5):
            signals.append(FraudSignal(
                15, 'Payment initiated within 5 seconds of booking (automated?)', 'timing'
            ))
        return signals

    # --- Helpers ---

    def _score_to_level(self, score):
        if score >= RISK_CRITICAL:
            return 'critical'
        elif score >= RISK_HIGH:
            return 'high'
        elif score >= RISK_MEDIUM:
            return 'medium'
        return 'low'

    def _determine_action(self, score):
        """Determine what action to take based on risk score."""
        if score >= RISK_CRITICAL:
            return 'block'  # Block the transaction
        elif score >= RISK_HIGH:
            return 'manual_review'  # Flag for admin review
        elif score >= RISK_MEDIUM:
            return 'monitor'  # Allow but monitor closely
        return 'allow'  # Normal processing


# Singleton instance
fraud_service = FraudDetectionService()
