package eu.lindenbaum.maven.erlang;

import java.util.Map;

public interface CheckRelResult {
  /**
   * Returns whether script execution was successfull.
   * 
   * @return {@code true} if script succeeded, {@code false} otherwise.
   */
  public boolean success();

  /**
   * Returns the release name as stated in the {@code .rel} file.
   * 
   * @return The release name or {@code "undefined"} if not present.
   */
  public String getName();

  /**
   * Returns the release version as stated in the {@code .rel} file.
   * 
   * @return The release version or {@code "undefined"} if not present.
   */
  public String getReleaseVersion();

  /**
   * Returns the erts version as stated in the {@code .rel} file.
   * 
   * @return The erts version or {@code "undefined"} if not present.
   */
  public String getErtsVersion();

  /**
   * Returns a map of the applications included in the release. The mapping is
   * application name to application version.
   * 
   * @return A non-{@code null} map containing the release applications.
   */
  public Map<String, String> getApplications();

}
