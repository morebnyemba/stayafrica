from django.apps import AppConfig

class PropertiesConfig(AppConfig):
    default_auto_field = 'django.db.models.BigAutoField'
    name = 'apps.properties'
    verbose_name = 'Properties'

    def ready(self):
        import apps.properties.signals  # noqa: F401
        # Import additional admin modules
        import apps.properties.analytics_admin  # noqa: F401
        import apps.properties.poi_admin  # noqa: F401
        import apps.properties.wishlist_admin  # noqa: F401
