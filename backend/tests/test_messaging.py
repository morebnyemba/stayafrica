"""
Comprehensive tests for the Messaging app.
Tests conversations, messages, and templates.
"""
import pytest
from django.urls import reverse
from rest_framework.test import APIClient
from apps.messaging.models import Conversation, Message


@pytest.mark.django_db
class TestConversationModel:
    """Tests for the Conversation model."""

    def test_conversation_creation(self, guest_user, host_user):
        conv = Conversation.objects.create()
        conv.participants.add(guest_user, host_user)
        assert conv.participants.count() == 2

    def test_conversation_string(self, guest_user, host_user):
        conv = Conversation.objects.create()
        conv.participants.add(guest_user, host_user)
        assert str(conv)  # Should not raise


@pytest.mark.django_db
class TestMessageModel:
    """Tests for the Message model."""

    def test_message_creation(self, guest_user, host_user):
        conv = Conversation.objects.create()
        conv.participants.add(guest_user, host_user)
        msg = Message.objects.create(
            conversation=conv,
            sender=guest_user,
            text='Hello, is this property available?',
        )
        assert msg.text == 'Hello, is this property available?'
        assert msg.sender == guest_user

    def test_message_read_status(self, guest_user, host_user):
        conv = Conversation.objects.create()
        conv.participants.add(guest_user, host_user)
        msg = Message.objects.create(
            conversation=conv,
            sender=guest_user,
            text='Check in question',
        )
        assert msg.is_read is False
        msg.is_read = True
        msg.save()
        msg.refresh_from_db()
        assert msg.is_read is True


@pytest.mark.django_db
class TestMessagingAPI:
    """Tests for Messaging API endpoints."""

    def setup_method(self):
        self.client = APIClient()

    def test_list_conversations_authenticated(self, guest_user):
        self.client.force_authenticate(user=guest_user)
        response = self.client.get(reverse('conversation-list'))
        assert response.status_code == 200

    def test_list_conversations_unauthenticated(self):
        response = self.client.get(reverse('conversation-list'))
        assert response.status_code in [401, 403]

    def test_send_message(self, guest_user, host_user):
        self.client.force_authenticate(user=guest_user)
        conv = Conversation.objects.create()
        conv.participants.add(guest_user, host_user)
        response = self.client.post(
            reverse('message-list'),
            {
                'conversation': conv.id,
                'text': 'Test message',
            },
            format='json',
        )
        assert response.status_code in [200, 201]

    def test_only_participants_see_conversation(self, user_factory):
        user1 = user_factory(email='p1@test.com')
        user2 = user_factory(email='p2@test.com')
        outsider = user_factory(email='outsider@test.com')
        conv = Conversation.objects.create()
        conv.participants.add(user1, user2)
        self.client.force_authenticate(user=outsider)
        response = self.client.get(reverse('conversation-list'))
        assert response.status_code == 200
        results = response.data.get('results', response.data)
        if isinstance(results, list):
            conv_ids = [c.get('id') for c in results]
            assert conv.id not in conv_ids
