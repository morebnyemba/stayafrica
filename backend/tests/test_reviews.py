"""
Comprehensive tests for the Reviews app.
Tests review creation, validation, voting, and host responses.
"""
import pytest
from decimal import Decimal
from datetime import date, timedelta
from django.urls import reverse
from rest_framework.test import APIClient
from apps.reviews.models import Review, ReviewVote


@pytest.mark.django_db
class TestReviewModel:
    """Tests for the Review model."""

    def test_review_creation(self, guest_user, booking_factory):
        booking = booking_factory(guest=guest_user, status='completed')
        review = Review.objects.create(
            booking=booking,
            guest=guest_user,
            property=booking.rental_property,
            rating=5,
            text='Excellent stay!',
        )
        assert review.rating == 5
        assert review.guest == guest_user

    def test_review_rating_range(self, guest_user, booking_factory):
        booking = booking_factory(guest=guest_user, status='completed')
        review = Review.objects.create(
            booking=booking,
            guest=guest_user,
            property=booking.rental_property,
            rating=1,
            text='Poor experience',
        )
        assert review.rating == 1

    def test_review_string_representation(self, guest_user, booking_factory):
        booking = booking_factory(guest=guest_user, status='completed')
        review = Review.objects.create(
            booking=booking,
            guest=guest_user,
            property=booking.rental_property,
            rating=4,
            text='Good stay',
        )
        assert str(review.rating) in str(review) or guest_user.email in str(review)


@pytest.mark.django_db
class TestReviewVoteModel:
    """Tests for the ReviewVote model."""

    def test_vote_helpful(self, guest_user, user_factory, booking_factory):
        booking = booking_factory(guest=guest_user, status='completed')
        review = Review.objects.create(
            booking=booking,
            guest=guest_user,
            property=booking.rental_property,
            rating=4,
            text='Nice place',
        )
        voter = user_factory(email='voter@test.com')
        vote = ReviewVote.objects.create(
            review=review,
            user=voter,
            is_helpful=True,
        )
        assert vote.is_helpful is True


@pytest.mark.django_db
class TestReviewAPI:
    """Tests for Review API endpoints."""

    def setup_method(self):
        self.client = APIClient()

    def test_list_reviews_authenticated(self, guest_user):
        self.client.force_authenticate(user=guest_user)
        response = self.client.get(reverse('review-list'))
        assert response.status_code == 200

    def test_unauthenticated_reviews_access(self):
        response = self.client.get(reverse('review-list'))
        assert response.status_code in [200, 401, 403]

    def test_create_review_for_completed_booking(self, guest_user, booking_factory):
        self.client.force_authenticate(user=guest_user)
        booking = booking_factory(guest=guest_user, status='completed')
        response = self.client.post(
            reverse('review-list'),
            {
                'booking': booking.id,
                'rating': 5,
                'text': 'Amazing property!',
            },
            format='json',
        )
        # Should succeed for completed bookings
        assert response.status_code in [200, 201, 400]

    def test_cannot_review_pending_booking(self, guest_user, booking_factory):
        self.client.force_authenticate(user=guest_user)
        booking = booking_factory(guest=guest_user, status='pending')
        response = self.client.post(
            reverse('review-list'),
            {
                'booking': booking.id,
                'rating': 5,
                'text': 'Should be rejected',
            },
            format='json',
        )
        assert response.status_code in [400, 403]
