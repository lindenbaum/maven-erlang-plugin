package eu.lindenbaum.maven.erlang;

import java.io.File;

/**
 * Interface representing the result returned by the {@link RuntimeInfoScript}.
 * 
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 */
public interface RuntimeInfo {
  /**
   * Returns the erlang runtime's library directory as returned by
   * <code>code:lib_dir()</code>.
   * 
   * @return The erlang runtime's library directory.
   */
  public File getLibDirectory();

  /**
   * Returns the erlang runtime version (ERTS version) of the backend node as
   * returned by <code>erlang:system_info(version)</code>.
   * 
   * @return The erlang runtime's system version.
   */
  public String getVersion();

  /**
   * Returns the erlang OTP release version of the backend node as returned by
   * <code>erlang:system_info(otp_release)</code>.
   * 
   * @return The erlang OTP release version string.
   */
  public String getOtpRelease();
}
