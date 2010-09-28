package eu.lindenbaum.maven;

import static eu.lindenbaum.maven.util.FileUtils.APP_FILTER;
import static eu.lindenbaum.maven.util.FileUtils.NULL_FILTER;
import static eu.lindenbaum.maven.util.FileUtils.copyDirectory;

import java.util.HashMap;
import java.util.Map;

import org.apache.maven.plugin.Mojo;
import org.apache.maven.plugin.MojoExecutionException;

/**
 * <p>
 * This {@link Mojo} copies the private resources to the
 * {@link AbstractErlangMojo#targetPriv} directory. Default source folders for
 * private resources are {@link AbstractErlangMojo#srcMainPriv} and
 * {@link AbstractErlangMojo#srcMainResources}.
 * </p>
 * <p>
 * Besides that this {@link Mojo} also copies the erlang application resource
 * file. The {@link Mojo} can also manage the application version. To use this
 * the user can set the application version in the .app file to
 * {@code ?APP_VERSION} which will be replaced with the project version
 * specified in the project pom.
 * </p>  
 * 
 * @goal copy-resources
 * @phase process-resources
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 */
public final class CopyResourcesMojo extends AbstractErlangMojo {
  @Override
  public void execute() throws MojoExecutionException {
    Map<String, String> replacements = new HashMap<String, String>();
    replacements.put("?APP_VERSION", "\"" + this.project.getVersion() + "\"");

    int copiedFiles = 0;
    copiedFiles += copyDirectory(this.srcMainPriv, this.targetPriv, NULL_FILTER);
    copiedFiles += copyDirectory(this.srcMainResources, this.targetPriv, NULL_FILTER);
    copiedFiles += copyDirectory(this.srcMainErlang, this.targetEbin, APP_FILTER, replacements);
    getLog().info("Copied " + copiedFiles + " files");
  }
}
