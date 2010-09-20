package eu.lindenbaum.maven;

import org.apache.maven.plugin.logging.Log;
import org.codehaus.plexus.logging.Logger;

public final class MavenPlexusLogger implements Logger {
  private final Log mLog;
  private final String mName;

  public MavenPlexusLogger(Log inLog) {
    this(inLog, "maven:logger");
  }

  public MavenPlexusLogger(Log inLog, String inName) {
    this.mLog = inLog;
    this.mName = inName;
  }

  public void debug(String message) {
    this.mLog.debug(message);
  }

  public void debug(String message, Throwable throwable) {
    this.mLog.debug(message, throwable);
  }

  public void error(String message) {
    this.mLog.error(message);
  }

  public void error(String message, Throwable throwable) {
    this.mLog.error(message, throwable);
  }

  public void fatalError(String message) {
    this.mLog.error(message);
  }

  public void fatalError(String message, Throwable throwable) {
    this.mLog.error(message, throwable);
  }

  public Logger getChildLogger(String inName) {
    return new MavenPlexusLogger(this.mLog, this.mName + "." + inName);
  }

  public String getName() {
    return this.mName;
  }

  public int getThreshold() {
    if (this.mLog.isDebugEnabled()) {
      return Logger.LEVEL_DEBUG;
    }
    else if (this.mLog.isInfoEnabled()) {
      return Logger.LEVEL_INFO;
    }
    else if (this.mLog.isWarnEnabled()) {
      return Logger.LEVEL_WARN;
    }
    else if (this.mLog.isErrorEnabled()) {
      return Logger.LEVEL_ERROR;
    }

    return Logger.LEVEL_DISABLED;
  }

  public void info(String message) {
    this.mLog.info(message);
  }

  public void info(String message, Throwable throwable) {
    this.mLog.info(message, throwable);
  }

  public boolean isDebugEnabled() {
    return this.mLog.isDebugEnabled();
  }

  public boolean isErrorEnabled() {
    return this.mLog.isErrorEnabled();
  }

  public boolean isFatalErrorEnabled() {
    return this.mLog.isErrorEnabled();
  }

  public boolean isInfoEnabled() {
    return this.mLog.isInfoEnabled();
  }

  public boolean isWarnEnabled() {
    return this.mLog.isWarnEnabled();
  }

  public void warn(String message) {
    this.mLog.warn(message);
  }

  public void warn(String message, Throwable throwable) {
    this.mLog.warn(message, throwable);
  }
}
