package eu.lindenbaum.maven;

import java.io.File;
import java.util.Collection;

/**
 * Represents a bean interface holding the directory layout of a project's
 * source directory (project sources/headers and resources go here).
 * 
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 * @since 2.1.0
 */
public interface SourceLayout {
  /**
   * Returns the base project directory.
   */
  public File base();

  // applications (erlang-std/erlang-otp) *************************************/

  /**
   * Returns the directory where the application (upgrade) files reside.
   */
  public File ebin();

  /**
   * Returns the application resource file of an application project. The
   * returned {@link File} is not guaranteed to exist.
   */
  public File appFile();

  /**
   * Returns the application upgrade file of an application project. The
   * returned {@link File} is not guaranteed to exist.
   */
  public File appupFile();

  /**
   * Returns the directory where header files reside.
   */
  public File include();

  /**
   * Returns the directory where the private resources reside.
   */
  public File priv();

  /**
   * Returns the directory where the erlang sources reside.
   */
  public File src();

  /**
   * Returns the directory where MIB soruces reside.
   * 
   * @since 2.2.0
   */
  public File mibs();

  /**
   * Returns the directory where erlang include files for tests reside.
   */
  public File testInclude();

  /**
   * Returns the directory where private test resources reside.
   */
  public File testPriv();

  /**
   * Returns the directory where the erlang test source files reside.
   */
  public Collection<File> testSrcs();

  /**
   * Returns the path to the application overview file
   * <code>overview.edoc</code>.
   */
  public File overviewEdoc();

  // release (erlang-rel) *****************************************************/

  /**
   * Returns the release file of a release project. The returned {@link File} is
   * not guaranteed to exist.
   */
  public File relFile();

  /**
   * Returns the release upgrade file of a release project. The returned
   * {@link File} is not guaranteed to exist.
   */
  public File relupFile();

  /**
   * Returns the system configuration file of a release project. The returned
   * {@link File} is not guaranteed to exist.
   */
  public File sysConfigFile();
}
