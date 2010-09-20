package eu.lindenbaum.maven.util;

import org.apache.maven.plugin.logging.Log;

/**
 * Containing utilities related to logging.
 * 
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 */
public final class LoggingUtils {
  /**
   * Logs a message using {@link Log#debug(CharSequence)} in case debug logging is enabled.
   * 
   * @param log logger to use
   * @param message message to log
   */
  public static void logDebug(Log log, String message) {
    if (log.isDebugEnabled()) {
      log.debug(message);
    }
  }
}
