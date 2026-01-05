FROM erlang:24-alpine

WORKDIR /app

# Install build tools
RUN apk add --no-cache git curl

# Copy application files
COPY rebar.config .
COPY src/ src/

# Download dependencies and compile
RUN rebar3 compile

# Build release
RUN rebar3 as prod release

# Expose HTTP API port
EXPOSE 8765

# Set environment variables
ENV DJANGO_API_URL=http://backend:8000
ENV HTTP_PORT=8765

# Start the service
CMD ["_build/prod/rel/messaging_service/bin/messaging_service", "foreground"]
