# Django Unfold Admin - Quick Start Guide for Developers

## For New Developers

This guide helps you quickly understand and work with the enhanced Django admin interface using Unfold.

---

## 1. Understanding the Structure

### Admin Class Template

```python
from django.contrib import admin
from django.utils.html import format_html
from django.utils.translation import gettext_lazy as _
from unfold.admin import ModelAdmin as UnfoldModelAdmin
from unfold.decorators import display
from apps.yourapp.models import YourModel


@admin.register(YourModel)
class YourModelAdmin(UnfoldModelAdmin):
    """Enhanced admin interface for YourModel"""
    
    # What users see in list view
    list_display = ['field1', 'field2', 'custom_badge', 'created_at']
    
    # Filters on the sidebar
    list_filter = ['status', 'created_at']
    
    # Search functionality
    search_fields = ['name', 'description', 'user__email']
    
    # Fields that can't be edited
    readonly_fields = ['created_at', 'updated_at', 'summary']
    
    # Optimize queries
    list_select_related = ['user', 'category']
    
    # Pagination
    list_per_page = 25
    
    # Actions in dropdown
    actions = ['activate', 'deactivate']
    
    # Organize detail view
    fieldsets = (
        (_('Basic Info'), {
            'fields': ('name', 'description'),
            'classes': ['tab'],
        }),
        (_('Settings'), {
            'fields': ('status', 'is_active'),
            'classes': ['tab'],
        }),
    )
    
    # Custom badge display
    @display(description=_('Status'), ordering='status', label=True)
    def custom_badge(self, obj):
        colors = {'active': 'success', 'pending': 'warning'}
        return {
            'value': obj.get_status_display(),
            'color': colors.get(obj.status, 'secondary'),
        }
    
    # Custom action
    @admin.action(description=_('Activate selected items'))
    def activate(self, request, queryset):
        updated = queryset.update(status='active')
        self.message_user(request, f'{updated} item(s) activated.')
```

---

## 2. StayAfrica Color Reference

**Quick Copy-Paste Colors:**

```python
# For summary box borders (choose one):
'#D9B168'  # Safari Gold - use for highlights, pricing
'#3A5C50'  # Moss Green - use for general info
'#122F26'  # Deep Forest - use for important notices

# For backgrounds:
'#F4F1EA'  # Ivory Sand - always use this
'#FFFFFF'  # Pure White - for nested boxes

# For text:
'#122F26'  # Deep Forest - for headings/strong text
'#3A5C50'  # Moss Green - for body text
'#0A1A15'  # Savanna Text - for subtle text
```

---

## 3. Creating Summary Boxes

### Basic Summary Template

```python
@display(description=_('Summary'))
def item_summary(self, obj):
    if obj.id:
        summary = f"""
        <div style="padding: 12px; background: #F4F1EA; 
                    border-left: 4px solid #D9B168; border-radius: 4px;">
            <strong style="color: #122F26; font-size: 16px;">
                Item Summary
            </strong><br/>
            <div style="margin-top: 8px; color: #3A5C50;">
                <strong>Name:</strong> {obj.name}<br/>
                <strong>Status:</strong> {obj.get_status_display()}<br/>
                <strong>Created:</strong> {obj.created_at.strftime('%B %d, %Y')}
            </div>
        </div>
        """
        return format_html(summary)
    return "Save to see summary"
```

### Grid Layout Summary

```python
@display(description=_('Dashboard'))
def dashboard_summary(self, obj):
    if obj.id:
        summary = f"""
        <div style="padding: 15px; background: #F4F1EA; 
                    border-left: 4px solid #D9B168; border-radius: 6px;">
            <h3 style="color: #122F26; margin: 0 0 15px 0;">
                Dashboard
            </h3>
            <div style="display: grid; grid-template-columns: repeat(2, 1fr); gap: 12px;">
                <div style="background: white; padding: 12px; border-radius: 4px; 
                            border-left: 3px solid #10B981;">
                    <div style="font-size: 24px; font-weight: bold; color: #10B981;">
                        {obj.total_count:,}
                    </div>
                    <div style="color: #3A5C50;">Total Items</div>
                </div>
                <div style="background: white; padding: 12px; border-radius: 4px; 
                            border-left: 3px solid #D9B168;">
                    <div style="font-size: 24px; font-weight: bold; color: #D9B168;">
                        {obj.active_count:,}
                    </div>
                    <div style="color: #3A5C50;">Active Items</div>
                </div>
            </div>
        </div>
        """
        return format_html(summary)
    return "Dashboard will appear after saving"
```

---

## 4. Badge Types & Usage

### Standard Status Badge

```python
@display(description=_('Status'), ordering='status', label=True)
def status_badge(self, obj):
    colors = {
        'active': 'success',      # Green
        'pending': 'warning',     # Gold/Yellow
        'failed': 'danger',       # Red
        'processing': 'info',     # Blue
        'archived': 'secondary',  # Gray
    }
    return {
        'value': obj.get_status_display(),
        'color': colors.get(obj.status, 'secondary'),
    }
```

### Boolean Badge

```python
@display(description=_('Active'), label=True, boolean=True)
def active_badge(self, obj):
    if obj.is_active:
        return {'value': 'Active', 'color': 'success'}
    return {'value': 'Inactive', 'color': 'danger'}
```

### Custom Text Badge

```python
@display(description=_('Priority'), label=True)
def priority_badge(self, obj):
    colors = {'high': 'danger', 'medium': 'warning', 'low': 'info'}
    return {
        'value': obj.priority.upper(),
        'color': colors.get(obj.priority, 'secondary'),
    }
```

---

## 5. Working with Inlines

### Tabular Inline (Recommended)

```python
from unfold.admin import TabularInline as UnfoldTabularInline

class ItemImageInline(UnfoldTabularInline):
    model = ItemImage
    extra = 1
    readonly_fields = ['image_preview', 'created_at']
    fields = ['image', 'image_preview', 'order', 'created_at']
    
    @display(description=_('Preview'))
    def image_preview(self, obj):
        if obj.image:
            return format_html(
                '<img src="{}" style="max-height: 100px; '
                'border-radius: 4px; border: 2px solid #D9B168;" />',
                obj.image.url
            )
        return '-'
```

### Stacked Inline (For Complex Forms)

```python
from unfold.admin import StackedInline as UnfoldStackedInline

class ItemDetailInline(UnfoldStackedInline):
    model = ItemDetail
    extra = 0
    fieldsets = (
        ('Details', {
            'fields': ('field1', 'field2', 'field3'),
        }),
    )
```

---

## 6. Custom Actions

### Basic Action

```python
@admin.action(description=_('Activate selected items'))
def activate_items(self, request, queryset):
    updated = queryset.update(is_active=True)
    self.message_user(request, f'{updated} item(s) activated.')
```

### Action with Conditions

```python
@admin.action(description=_('Process selected items'))
def process_items(self, request, queryset):
    count = 0
    for item in queryset:
        if item.can_be_processed():
            item.process()
            count += 1
    
    self.message_user(
        request, 
        f'{count} of {queryset.count()} item(s) processed.',
        level='success' if count > 0 else 'warning'
    )
```

### Action with Confirmation

```python
@admin.action(description=_('Delete selected items (careful!)'))
def delete_items(self, request, queryset):
    # Django will automatically show confirmation page
    queryset.delete()
    self.message_user(request, 'Items deleted successfully.')
```

---

## 7. Fieldset Organization

### Using Tabs

```python
fieldsets = (
    (_('Basic Information'), {
        'fields': ('name', 'description', 'category'),
        'classes': ['tab'],  # Creates a tab
    }),
    (_('Advanced Settings'), {
        'fields': ('setting1', 'setting2'),
        'classes': ['tab'],  # Another tab
    }),
    (_('Metadata'), {
        'fields': ('created_at', 'updated_at'),
        'classes': ['collapse', 'tab'],  # Collapsed tab
    }),
)
```

### With Descriptions

```python
fieldsets = (
    (_('Payment Settings'), {
        'fields': ('stripe_key', 'stripe_secret'),
        'classes': ['tab'],
        'description': 'Configure Stripe payment gateway settings',
    }),
)
```

---

## 8. Search & Filters

### Optimized Search

```python
# Search across multiple fields including related models
search_fields = [
    'name',                    # Direct field
    'description',             # Direct field
    'user__email',            # Related model
    'user__username',         # Related model
    'category__name',         # Related model
]
```

### Custom Filters

```python
list_filter = [
    'status',                 # Choice field
    'is_active',             # Boolean field
    'created_at',            # Date field
    'category',              # Foreign key
    ('price', admin.EmptyFieldListFilter),  # Custom filter
]
```

---

## 9. Performance Tips

### Use select_related

```python
list_select_related = ['user', 'category', 'created_by']
```

### Use prefetch_related

```python
def get_queryset(self, request):
    qs = super().get_queryset(request)
    return qs.prefetch_related('tags', 'images')
```

### Limit Queries in Display Methods

```python
@display(description=_('Item Count'))
def item_count(self, obj):
    # Bad: triggers query for each row
    # return obj.items.count()
    
    # Good: use annotation
    return obj.item_count  # Add in get_queryset
```

---

## 10. Common Patterns

### Display Money

```python
@display(description=_('Price'), ordering='price')
def price_display(self, obj):
    return f"{obj.currency} {obj.price:.2f}"
```

### Display Date

```python
@display(description=_('Created'))
def created_display(self, obj):
    return obj.created_at.strftime('%B %d, %Y at %H:%M')
```

### Display Related Count

```python
@display(description=_('Items'))
def item_count(self, obj):
    return f"{obj.items.count()} items"
```

### Display with Icon

```python
@display(description=_('Type'))
def type_display(self, obj):
    icons = {'pdf': '📄', 'image': '🖼️', 'video': '🎥'}
    icon = icons.get(obj.file_type, '📁')
    return f"{icon} {obj.get_file_type_display()}"
```

---

## 11. Debugging Tips

### Print Queries

```python
def get_queryset(self, request):
    qs = super().get_queryset(request)
    print(qs.query)  # See the SQL
    return qs
```

### Check Template Context

```python
def changelist_view(self, request, extra_context=None):
    extra_context = extra_context or {}
    print(extra_context)  # See what's available
    return super().changelist_view(request, extra_context)
```

---

## 12. Testing Your Admin

### Manual Testing Checklist

- [ ] List view displays correctly
- [ ] Badges show proper colors
- [ ] Search works as expected
- [ ] Filters work correctly
- [ ] Actions complete successfully
- [ ] Detail view tabs work
- [ ] Inlines save properly
- [ ] Summary boxes render correctly
- [ ] Responsive on mobile
- [ ] No performance issues

### Python Shell Testing

```bash
python manage.py shell

from apps.yourapp.models import YourModel
from apps.yourapp.admin import YourModelAdmin
from django.contrib.admin.sites import AdminSite

# Test admin class
admin_instance = YourModelAdmin(YourModel, AdminSite())
obj = YourModel.objects.first()

# Test display methods
print(admin_instance.status_badge(obj))
print(admin_instance.item_summary(obj))
```

---

## 13. Common Issues & Solutions

### Issue: Badge Not Showing

```python
# Make sure label=True is set
@display(description='Status', label=True)  # ← This is required!
def status_badge(self, obj):
    return {'value': 'Active', 'color': 'success'}
```

### Issue: Summary Box Not Rendering

```python
# Make sure to use format_html
from django.utils.html import format_html

@display(description='Summary')
def summary(self, obj):
    html = "<div>...</div>"
    return format_html(html)  # ← This is required!
```

### Issue: Slow Admin Page

```python
# Add select_related and prefetch_related
list_select_related = ['user', 'category']

def get_queryset(self, request):
    return super().get_queryset(request).select_related(
        'user', 'category'
    ).prefetch_related('tags')
```

---

## 14. Resources

### Documentation
- Django Admin: https://docs.djangoproject.com/en/5.0/ref/contrib/admin/
- Unfold: https://github.com/unfoldadmin/django-unfold
- StayAfrica Colors: See `BRAND_COLORS.md`

### Files to Reference
- `ADMIN_THEME_UPDATE_SUMMARY.md` - Comprehensive change log
- `ADMIN_VISUAL_IMPROVEMENTS.md` - Visual examples
- Any `apps/*/admin.py` - Working examples

---

## 15. Getting Help

1. Check existing admin.py files for patterns
2. Review this quick start guide
3. Read the comprehensive documentation
4. Ask team members
5. Check Django/Unfold documentation

---

**Remember**: Keep it consistent with the StayAfrica brand colors and follow existing patterns for a cohesive admin experience!

**Last Updated**: January 11, 2026
**Version**: 1.0
