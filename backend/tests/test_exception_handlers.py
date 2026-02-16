"""
Tests for the custom exception handlers and request logging middleware.
"""
import pytest
from unittest.mock import MagicMock, patch
from django.http import HttpRequest, HttpResponse, JsonResponse
from rest_framework.exceptions import NotFound, PermissionDenied, ValidationError
from rest_framework import status

from utils.exception_handlers import custom_exception_handler, RequestLoggingMiddleware


class TestCustomExceptionHandler:
    """Test the DRF custom exception handler."""

    def test_handles_known_drf_exception(self):
        """Known DRF exceptions should return a structured error response."""
        exc = NotFound('Resource not found')
        context = {'view': MagicMock()}

        response = custom_exception_handler(exc, context)

        assert response.status_code == 404
        assert response.data['success'] is False
        assert response.data['error']['status_code'] == 404

    def test_handles_validation_error(self):
        """Validation errors should be wrapped in the standard format."""
        exc = ValidationError({'email': ['This field is required.']})
        context = {'view': MagicMock()}

        response = custom_exception_handler(exc, context)

        assert response.status_code == 400
        assert response.data['success'] is False

    def test_handles_permission_denied(self):
        """Permission denied should return 403 in standard format."""
        exc = PermissionDenied('Not allowed')
        context = {'view': MagicMock()}

        response = custom_exception_handler(exc, context)

        assert response.status_code == 403
        assert response.data['success'] is False

    @patch('utils.exception_handlers.logger')
    def test_handles_unexpected_exception(self, mock_logger):
        """Unhandled exceptions should return 500 and log the error."""
        exc = RuntimeError('Something broke')
        context = {'view': 'TestView'}

        response = custom_exception_handler(exc, context)

        assert response.status_code == 500
        assert response.data['success'] is False
        assert 'unexpected error' in response.data['error']['detail'].lower()
        mock_logger.error.assert_called_once()


class TestRequestLoggingMiddleware:
    """Test the request logging middleware."""

    def _make_request(self, method='GET', path='/api/test/'):
        request = HttpRequest()
        request.method = method
        request.path = path
        request.META['QUERY_STRING'] = ''
        return request

    def test_passes_through_successful_response(self):
        """2xx responses should pass through without logging errors."""
        get_response = MagicMock(return_value=HttpResponse(status=200))
        middleware = RequestLoggingMiddleware(get_response)
        request = self._make_request()

        response = middleware(request)

        assert response.status_code == 200

    @patch('utils.exception_handlers.logger')
    def test_logs_server_errors(self, mock_logger):
        """5xx responses should be logged as errors."""
        get_response = MagicMock(return_value=HttpResponse(status=500))
        middleware = RequestLoggingMiddleware(get_response)
        request = self._make_request()

        response = middleware(request)

        assert response.status_code == 500
        mock_logger.error.assert_called_once()

    @patch('utils.exception_handlers.logger')
    def test_logs_client_errors(self, mock_logger):
        """4xx responses should be logged as warnings."""
        get_response = MagicMock(return_value=HttpResponse(status=404))
        middleware = RequestLoggingMiddleware(get_response)
        request = self._make_request()

        response = middleware(request)

        assert response.status_code == 404
        mock_logger.warning.assert_called_once()

    @patch('utils.exception_handlers.logger')
    def test_process_exception_returns_json(self, mock_logger):
        """Unhandled exceptions should return a JSON error response."""
        get_response = MagicMock()
        middleware = RequestLoggingMiddleware(get_response)
        request = self._make_request()

        response = middleware.process_exception(request, RuntimeError('crash'))

        assert isinstance(response, JsonResponse)
        assert response.status_code == 500
        mock_logger.error.assert_called_once()
