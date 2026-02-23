# Admin Authentication Logic

## Backend Implementation

### JWT Token Generation
Location: `backend/apps/users/serializers.py` (line 74-80)

```python
@classmethod
def get_token(cls, user):
    token = super().get_token(user)
    token['email'] = user.email
    token['role'] = user.role  # ← Includes 'admin', 'host', or 'guest'
    token['is_verified'] = user.is_verified
    return token
```

### User Model Admin Check
Location: `backend/apps/users/models.py` (line 56-58)

```python
@property
def is_admin_user(self):
    return self.role == 'admin' or self.is_staff
```

The backend considers a user an admin if EITHER:
- `role = 'admin'` (stored in database), OR
- `is_staff = True` (Django superuser flag)

## Frontend Implementation

### Middleware Protection
Location: `web/src/middleware.ts` (line 20-30)

```typescript
function isUserAdmin(token: string): boolean {
  try {
    const payload = JSON.parse(Buffer.from(token.split('.')[1], 'base64').toString());
    // User is admin if role is 'admin' OR is_staff is true (matches backend logic)
    return payload.role === 'admin' || payload.is_staff === true;
  } catch (error) {
    return false;
  }
}
```

The middleware checks BOTH fields to match the backend's logic.

## Admin Access Methods

### Method 1: Regular Admin User (role='admin')
Create a user with admin role:

```python
from apps.users.models import User

admin = User.objects.create_user(
    email='admin@stayafrica.com',
    username='admin',
    password='secure_password',
    first_name='Admin',
    last_name='User',
    role='admin',  # ← Sets role to admin
    is_verified=True
)
```

JWT Token will contain: `{ role: 'admin', email: '...', is_verified: true }`

### Method 2: Django Superuser (is_staff=True)
Create a superuser via Django command:

```bash
python manage.py createsuperuser
```

Or via shell:
```python
from apps.users.models import User

superuser = User.objects.create_superuser(
    email='super@stayafrica.com',
    username='superadmin',
    password='secure_password',
    first_name='Super',
    last_name='Admin'
)
# Automatically sets: is_staff=True, is_superuser=True
```

**Note:** Currently, `is_staff` is NOT included in the JWT token by default. To support this method fully, add to `CustomTokenObtainPairSerializer.get_token()`:

```python
token['is_staff'] = user.is_staff
```

## Current Behavior

### Without is_staff in JWT
- ✅ Users with `role='admin'` can access admin portal
- ❌ Django superusers (is_staff=True) cannot access admin portal (unless role is also set to 'admin')

### With is_staff in JWT (if added)
- ✅ Users with `role='admin'` can access admin portal
- ✅ Django superusers (is_staff=True) can access admin portal
- ✅ Perfect alignment with backend's `is_admin_user` property

## Recommendation

For complete parity with backend logic, add `is_staff` to the JWT token:

```python
# In backend/apps/users/serializers.py
@classmethod
def get_token(cls, user):
    token = super().get_token(user)
    token['email'] = user.email
    token['role'] = user.role
    token['is_verified'] = user.is_verified
    token['is_staff'] = user.is_staff  # ← Add this line
    return token
```

This ensures:
- Django superusers created via `createsuperuser` have admin portal access
- Regular users with `role='admin'` have admin portal access
- Frontend middleware logic perfectly matches backend `is_admin_user` property

## Security Considerations

1. **JWT is decoded, not verified:** Middleware reads the token payload without cryptographic verification. This is acceptable for UI routing since the backend APIs verify tokens on every request.

2. **Backend always validates:** All admin API endpoints should use `IsAdminUser` permission or check `user.is_admin_user` to ensure authorization.

3. **Token expiration:** JWTs expire based on `SIMPLE_JWT` settings. Expired tokens are rejected by backend APIs.

4. **Role changes:** If a user's role changes, they must re-login to get a new JWT with updated claims.

## Testing Admin Access

1. **Create an admin user:**
   ```bash
   python manage.py shell
   >>> from apps.users.models import User
   >>> User.objects.create_user(email='test@admin.com', username='testadmin', password='test123', role='admin')
   ```

2. **Login via frontend:**
   - Navigate to `/login`
   - Enter credentials
   - JWT token is stored in localStorage and cookies

3. **Access admin portal:**
   - Navigate to `/admin`
   - Middleware checks token
   - If authorized, admin portal loads
   - If unauthorized, redirects to dashboard with error

4. **Test API access:**
   ```bash
   curl -H "Authorization: Bearer <token>" http://localhost:8000/api/v1/admin/stats/dashboard/
   ```

## Summary

- ✅ **Current implementation works** for users with `role='admin'`
- ✅ **Middleware updated** to check both `role` and `is_staff` (commit 52c913f)
- ⚠️ **Optional enhancement:** Add `is_staff` to JWT token for full superuser support
- ✅ **Security:** Backend APIs validate tokens and permissions independently
