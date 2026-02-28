from django.apps import AppConfig


class AdminDashboardConfig(AppConfig):
    default_auto_field = 'django.db.models.BigAutoField'
    name = 'apps.admin_dashboard'
    verbose_name = 'Admin Dashboard'

    def ready(self):
        self._patch_unfold_flatten_context()

    @staticmethod
    def _patch_unfold_flatten_context():
        """
        Monkey-patch unfold's _flatten_context to handle Context objects
        that end up in context.dicts (Django bug: _reset_dicts appends
        Context objects directly instead of flattening them like push() does).

        Without this patch, admin change-form pages crash with:
            ValueError: dictionary update sequence element #0 has length N;
                        2 is required
        because submit_row (InclusionAdminNode) returns a Context, which
        InclusionNode.render() passes to context.new(), and _reset_dicts
        appends it raw.  When _flatten_context later calls context.flatten(),
        dict.update(Context) fails.
        """
        try:
            from django.template.context import BaseContext
            import unfold.templatetags.unfold as unfold_tags

            def _safe_flatten_context(context):
                flat = {}
                for d in context.dicts:
                    if isinstance(d, BaseContext):
                        # Recursively flatten nested Context objects
                        flat.update(_safe_flatten_context(d))
                    elif hasattr(d, 'keys'):
                        flat.update(d)
                    # else: skip non-dict, non-context layers
                return flat

            unfold_tags._flatten_context = _safe_flatten_context
        except ImportError:
            pass  # unfold not installed
