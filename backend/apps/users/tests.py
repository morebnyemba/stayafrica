from django.test import TestCase
from apps.users.models import User

class UserModelTest(TestCase):
    def setUp(self):
        self.user = User.objects.create_user(
            email='test@example.com',
            username='testuser',
            password='testpass123'
        )
    
    def test_user_creation(self):
        self.assertTrue(User.objects.filter(email='test@example.com').exists())
    
    def test_user_is_guest_by_default(self):
        self.assertEqual(self.user.role, 'guest')
