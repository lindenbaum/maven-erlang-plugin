package eu.lindenbaum.maven.erlang;

import java.util.List;

/**
 * Interface representing the result returned by the {@link CheckAppScript}.
 * 
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 */
public interface CheckAppResult {
  /**
   * Returns the application name as stated in the {@code .app} file.
   * 
   * @return The application name or {@code "undefined"} if not present.
   */
  public String getName();

  /**
   * Returns the application version as stated in the {@code .app} file.
   * 
   * @return The application version or {@code "undefined"} if not present.
   */
  public String getVersion();

  /**
   * Returns the applications start module as stated in the {@code .app} file.
   * 
   * @return The start module name or {@code "omitted"} if the application seems
   *         to be a library application.
   */
  public String getStartModule();

  /**
   * Returns a list of the modules configured in the {@code .app} file.
   * 
   * @return A non-{@code null} list containing the configured applications
   *         modules.
   */
  public List<String> getModules();

  /**
   * Returns a list of the applications that must be running before the
   * application can start as stated in the {@code .app} file.
   * 
   * @return A non-{@code null} list containing the application dependencies.
   */
  public List<String> getApplications();
}
