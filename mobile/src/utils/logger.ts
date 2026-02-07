/**
 * Centralized logging service for the mobile app
 * In production, this can be integrated with error tracking services like Sentry
 */

type LogLevel = 'debug' | 'info' | 'warn' | 'error';

interface LogEntry {
  level: LogLevel;
  message: string;
  timestamp: Date;
  context?: any;
  error?: Error;
}

class Logger {
  private isDevelopment = __DEV__;
  private logs: LogEntry[] = [];
  private maxLogs = 100; // Keep last 100 logs in memory

  /**
   * Log a debug message (only in development)
   */
  debug(message: string, context?: any): void {
    if (this.isDevelopment) {
      this.log('debug', message, context);
      console.log(`[DEBUG] ${message}`, context || '');
    }
  }

  /**
   * Log an info message
   */
  info(message: string, context?: any): void {
    this.log('info', message, context);
    if (this.isDevelopment) {
      console.info(`[INFO] ${message}`, context || '');
    }
  }

  /**
   * Log a warning
   */
  warn(message: string, context?: any): void {
    this.log('warn', message, context);
    console.warn(`[WARN] ${message}`, context || '');
  }

  /**
   * Log an error
   */
  error(message: string, error?: Error | any, context?: any): void {
    this.log('error', message, context, error);
    console.error(`[ERROR] ${message}`, {
      error: error?.message || error,
      stack: error?.stack,
      ...context,
    });

    // In production, send to error tracking service (e.g., Sentry)
    if (!this.isDevelopment) {
      this.sendToErrorTracking(message, error, context);
    }
  }

  /**
   * Log an API error with standardized format
   */
  apiError(endpoint: string, error: any, context?: any): void {
    const message = `API Error: ${endpoint}`;
    const errorDetails = {
      status: error?.response?.status,
      statusText: error?.response?.statusText,
      data: error?.response?.data,
      message: error?.message,
      ...context,
    };
    
    this.error(message, error, errorDetails);
  }

  /**
   * Store log entry in memory
   */
  private log(level: LogLevel, message: string, context?: any, error?: Error): void {
    const entry: LogEntry = {
      level,
      message,
      timestamp: new Date(),
      context,
      error,
    };

    this.logs.push(entry);

    // Keep only the last maxLogs entries
    if (this.logs.length > this.maxLogs) {
      this.logs.shift();
    }
  }

  /**
   * Send error to tracking service (Sentry, Bugsnag, etc.)
   */
  private sendToErrorTracking(message: string, error: Error | any, context?: any): void {
    // TODO: Integrate with error tracking service
    // Example for Sentry:
    // Sentry.captureException(error, {
    //   extra: {
    //     message,
    //     ...context,
    //   },
    // });
  }

  /**
   * Get recent logs (useful for debugging)
   */
  getRecentLogs(count: number = 50): LogEntry[] {
    return this.logs.slice(-count);
  }

  /**
   * Clear all stored logs
   */
  clearLogs(): void {
    this.logs = [];
  }

  /**
   * Export logs as JSON string (useful for support tickets)
   */
  exportLogs(): string {
    return JSON.stringify(this.logs, null, 2);
  }
}

// Export singleton instance
export const logger = new Logger();

// Convenience exports
export const logDebug = logger.debug.bind(logger);
export const logInfo = logger.info.bind(logger);
export const logWarn = logger.warn.bind(logger);
export const logError = logger.error.bind(logger);
export const logApiError = logger.apiError.bind(logger);
