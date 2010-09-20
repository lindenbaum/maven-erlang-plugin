package eu.lindenbaum.maven;

import org.apache.maven.plugin.logging.Log;
import org.codehaus.plexus.logging.Logger;

public final class MavenPlexusLogger implements Logger {
  private final Log log;
  private final String name;

  public MavenPlexusLogger(Log log) {
    this(log, "maven:logger");
  }

  public MavenPlexusLogger(Log log, String name) {
    this.log = log;
    this.name = name;
  }

  public void debug(String message) {
    this.log.debug(message);
  }

  public void debug(String message, Throwable throwable) {
    this.log.debug(message, throwable);
  }

  public void error(String message) {
    this.log.error(message);
  }

  public void error(String message, Throwable throwable) {
    this.log.error(message, throwable);
  }

  public void fatalError(String message) {
    this.log.error(message);
  }

  public void fatalError(String message, Throwable throwable) {
    this.log.error(message, throwable);
  }

  public Logger getChildLogger(String name) {
    return new MavenPlexusLogger(this.log, this.name + "." + name);
  }

  public String getName() {
    return this.name;
  }

  public int getThreshold() {
    if (this.log.isDebugEnabled()) {
      return Logger.LEVEL_DEBUG;
    }
    else if (this.log.isInfoEnabled()) {
      return Logger.LEVEL_INFO;
    }
    else if (this.log.isWarnEnabled()) {
      return Logger.LEVEL_WARN;
    }
    else if (this.log.isErrorEnabled()) {
      return Logger.LEVEL_ERROR;
    }

    return Logger.LEVEL_DISABLED;
  }

  public void info(String message) {
    this.log.info(message);
  }

  public void info(String message, Throwable throwable) {
    this.log.info(message, throwable);
  }

  public boolean isDebugEnabled() {
    return this.log.isDebugEnabled();
  }

  public boolean isErrorEnabled() {
    return this.log.isErrorEnabled();
  }

  public boolean isFatalErrorEnabled() {
    return this.log.isErrorEnabled();
  }

  public boolean isInfoEnabled() {
    return this.log.isInfoEnabled();
  }

  public boolean isWarnEnabled() {
    return this.log.isWarnEnabled();
  }

  public void warn(String message) {
    this.log.warn(message);
  }

  public void warn(String message, Throwable throwable) {
    this.log.warn(message, throwable);
  }
}
