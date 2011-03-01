package eu.lindenbaum.maven.erlang;

import eu.lindenbaum.maven.util.ErlConstants;
import eu.lindenbaum.maven.util.FileUtils;

import org.apache.maven.plugin.MojoExecutionException;

/**
 * Abstract base class for erlang {@link Script}s that loads the corresponding
 * erlang script file into a {@link String} on object creation.
 * 
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 * @param <T> the return type of the {@link Script}
 */
abstract class AbstractScript<T> implements Script<T> {
  protected final String script;

  /**
   * This will load the erlang script file for this {@link Script} from the
   * classpath. It is assumed that the erlang script file can be located in the
   * subclasses package path and has the name of the subclass in lower case with
   * the erl extension appended. For example the erlang script file's classpath
   * url for the java class {@code eu.lindenbaum.maven.erlang.Example} would be
   * {@code /eu/lindenbaum/maven/erlang/example.erl}.
   * 
   * @throws MojoExecutionException
   */
  public AbstractScript() throws MojoExecutionException {
    String path = "/" + getClass().getPackage().getName().replace(".", "/");
    String name = getClass().getSimpleName().toLowerCase() + ErlConstants.ERL_SUFFIX;
    this.script = FileUtils.readFileFromClassPath(getClass(), path, name);
  }
}
