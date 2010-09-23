package eu.lindenbaum.maven;

import static eu.lindenbaum.maven.util.FileUtils.APP_FILTER;
import static eu.lindenbaum.maven.util.FileUtils.NULL_FILTER;
import static eu.lindenbaum.maven.util.FileUtils.copyDirectory;

import java.util.HashMap;
import java.util.Map;

import org.apache.maven.plugin.MojoExecutionException;

/**
 * Copy resources to target/priv.
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
