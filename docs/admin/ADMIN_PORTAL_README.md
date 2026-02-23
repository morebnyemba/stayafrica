# StayAfrica Admin Portal

## Overview

The admin portal is a comprehensive administrative interface for managing the StayAfrica platform. It provides system analytics, user management, property oversight, booking monitoring, payment tracking, and system configuration.

## Access

**URL:** `/admin`

**Authentication:** Only users with the `admin` role can access the portal.

## Features

### 1. Dashboard (`/admin`)
- **System Analytics:** Real-time statistics on revenue, users, hosts, properties, and bookings
- **Trend Charts:** Visual representations of bookings and revenue over time
- **Property Distribution:** Pie chart showing property types
- **Recent Activity:** Live feed of platform activities

### 2. User Management (`/admin/users`)
- View all registered users with pagination
- Search users by name or email
- Filter users by role (Guest, Host, Admin)
- Change user roles
- Verify user accounts
- View user statistics by role and verification status

### 3. Property Management (`/admin/properties`)
- View all property listings
- Search properties by title or location
- Filter by status (Active, Pending Approval, Inactive)
- Approve or reject property listings
- Bulk approve multiple properties
- View property details and images

### 4. Booking Management (`/admin/bookings`)
- Monitor all bookings across the platform
- Search by booking reference
- Filter by status (Pending, Confirmed, Completed, Cancelled)
- View booking details including dates, guests, and amounts
- Track service fees and commissions

### 5. Payment Management (`/admin/payments`)
- View all payment transactions
- Filter by payment status
- Track payment providers (Paynow, PayFast, Stripe, Ozow)
- Monitor total revenue
- View payment gateway references

### 6. Audit Logs (`/admin/audit-logs`)
- Track all system activities
- View user actions and changes
- Filter by action type
- See detailed change logs
- Monitor system security

### 7. System Settings (`/admin/settings`)
- View pricing configuration (commission rate, service fee)
- Review business rules (booking limits, review windows)
- Check email configuration
- Monitor payment gateway status
- View maintenance mode status

## Security

### Role-Based Access Control
The middleware automatically:
- Checks for authentication tokens
- Verifies user role from JWT
- Redirects non-admin users to dashboard
- Redirects unauthenticated users to login

### Protected Routes
All `/admin/*` routes are protected and require:
1. Valid authentication token
2. User role set to `admin`

## API Integration

The admin portal integrates with backend API endpoints at:
- `/api/v1/admin/stats/dashboard/` - Dashboard statistics
- `/api/v1/admin/audit-logs/` - Audit logs
- `/api/v1/admin/config/fees/` - System configuration
- `/api/v1/users/` - User management
- `/api/v1/properties/` - Property management
- `/api/v1/bookings/` - Booking management
- `/api/v1/payments/` - Payment management

## Technology Stack

- **Framework:** Next.js 16 with App Router
- **UI Components:** React 18 with Tailwind CSS
- **Charts:** Recharts for data visualization
- **Icons:** Lucide React
- **Notifications:** React Hot Toast
- **State Management:** React hooks

## Layout Structure

```
/admin
├── layout.tsx              # Admin sidebar navigation and layout
├── page.tsx                # Dashboard with analytics
├── users/
│   └── page.tsx           # User management interface
├── properties/
│   └── page.tsx           # Property management interface
├── bookings/
│   └── page.tsx           # Booking management interface
├── payments/
│   └── page.tsx           # Payment management interface
├── audit-logs/
│   └── page.tsx           # Audit logs viewer
└── settings/
    └── page.tsx           # System settings viewer
```

## Design Features

- **Responsive Design:** Works on desktop and tablet devices
- **Real-time Updates:** Live data from the backend API
- **Intuitive Navigation:** Sidebar with clear sections
- **Data Visualization:** Charts and graphs for analytics
- **Search & Filters:** Easy data discovery and filtering
- **Pagination:** Efficient handling of large datasets
- **Status Badges:** Color-coded status indicators
- **Loading States:** Smooth loading animations

## Creating an Admin User

To create an admin user, use Django management:

```bash
# Via Django admin
python manage.py createsuperuser

# Or update an existing user via Django shell
python manage.py shell
>>> from apps.users.models import User
>>> user = User.objects.get(email='admin@example.com')
>>> user.role = 'admin'
>>> user.is_staff = True
>>> user.is_superuser = True
>>> user.save()
```

## Future Enhancements

- [ ] Analytics export (CSV, PDF)
- [ ] Email notifications for admin actions
- [ ] Advanced filtering and sorting
- [ ] Bulk actions for users and bookings
- [ ] Real-time notifications via WebSockets
- [ ] Admin activity dashboard
- [ ] System health monitoring
- [ ] Automated reports generation

## Support

For technical support or questions about the admin portal, contact the development team or refer to the main project documentation.
