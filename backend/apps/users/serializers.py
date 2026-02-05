from rest_framework import serializers
from apps.users.models import User, UserPreference, UserPropertyInteraction
from django.contrib.auth.hashers import make_password
from django.core.validators import RegexValidator
from rest_framework_simplejwt.serializers import TokenObtainPairSerializer

class UserSerializer(serializers.ModelSerializer):
    password = serializers.CharField(write_only=True, min_length=8)
    username = serializers.CharField(required=False)
    
    class Meta:
        model = User
        fields = [
            'id', 'email', 'username', 'first_name', 'last_name', 'phone_number',
            'role', 'country_of_residence', 'is_verified', 'profile_picture',
            'bio', 'password', 'is_online', 'last_seen', 'created_at', 'updated_at'
        ]
        read_only_fields = ['id', 'created_at', 'updated_at', 'is_verified', 'is_online', 'last_seen']
    
    def create(self, validated_data):
        password = validated_data.pop('password')
        # Generate username from email if not provided
        if 'username' not in validated_data or not validated_data['username']:
            email = validated_data.get('email', '')
            base_username = email.split('@')[0]
            validated_data['username'] = base_username
            # Handle username conflicts
            counter = 1
            while User.objects.filter(username=validated_data['username']).exists():
                validated_data['username'] = f"{base_username}{counter}"
                counter += 1
        
        user = User.objects.create_user(**validated_data)
        user.set_password(password)
        user.save()
        return user

class UserProfileSerializer(serializers.ModelSerializer):
    phone_number = serializers.CharField(
        required=False, 
        allow_blank=True,
        validators=[
            RegexValidator(
                regex=r'^(\+?1?\d{9,15})?$',  # Allow empty or valid phone
                message='Phone number must be between 9 and 15 digits when provided.'
            )
        ]
    )
    
    class Meta:
        model = User
        fields = [
            'id', 'email', 'username', 'first_name', 'last_name', 'phone_number',
            'role', 'country_of_residence', 'is_verified', 'profile_picture', 'bio',
            'is_online', 'last_seen'
        ]
        read_only_fields = ['id', 'email', 'username', 'role', 'is_verified', 'is_online', 'last_seen']

class CustomTokenObtainPairSerializer(TokenObtainPairSerializer):
    username_field = 'email'
    
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        # Allow either email or username
        self.fields['email'] = serializers.EmailField(required=False)
        self.fields['username'] = serializers.CharField(required=False)
    
    def validate(self, attrs):
        # Allow login with either email or username
        email = attrs.get('email')
        username = attrs.get('username')
        password = attrs.get('password')
        
        if not (email or username):
            raise serializers.ValidationError('Email or username is required')
        
        # If email provided, find username
        if email and not username:
            try:
                user = User.objects.get(email=email)
                attrs['username'] = user.username
            except User.DoesNotExist:
                raise serializers.ValidationError('No user found with this email')
        
        return super().validate(attrs)
    
    @classmethod
    def get_token(cls, user):
        token = super().get_token(user)
        token['email'] = user.email
        token['role'] = user.role
        token['is_verified'] = user.is_verified
        return token


class UserPreferenceSerializer(serializers.ModelSerializer):
    class Meta:
        model = UserPreference
        fields = [
            'id', 'preferred_property_types', 'preferred_min_price', 'preferred_max_price',
            'preferred_countries', 'preferred_cities', 'usual_guest_count',
            'preferred_amenities', 'last_latitude', 'last_longitude',
            'created_at', 'updated_at'
        ]
        read_only_fields = ['id', 'created_at', 'updated_at']


class UserPropertyInteractionSerializer(serializers.ModelSerializer):
    class Meta:
        model = UserPropertyInteraction
        fields = [
            'id', 'property_id', 'interaction_type', 'search_query',
            'viewed_duration_seconds', 'created_at'
        ]
        read_only_fields = ['id', 'created_at']

