package eu.lindenbaum.maven;

import static eu.lindenbaum.maven.util.ErlConstants.SRC_SUFFIX;
import static eu.lindenbaum.maven.util.ErlConstants.TARGZ_SUFFIX;
import static eu.lindenbaum.maven.util.ErlUtils.eval;
import static eu.lindenbaum.maven.util.FileUtils.getDependencies;
import static eu.lindenbaum.maven.util.FileUtils.getDirectoriesRecursive;

import java.io.File;
import java.io.FileFilter;
import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.plugin.Mojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugin.logging.Log;

/**
 * <p>
 * This {@link Mojo} packages creates a release {@code .tar.gz} using the erlang
 * {@code systools} modules {@code make_tar/2} function.
 * </p>
 * <p>
 * The build can be customized by providing additional options through the
 * {@link #tarOptions} parameter in the specific project pom. By default all
 * release dependecies will be packaged including their {@code ebin},
 * {@code include}, {@code mibs} and non-erlang source directories.
 * </p>
 * 
 * @goal package-release
 * @phase package
 * @author Paul Guyot
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 */
public final class PackageReleaseMojo extends AbstractErlangMojo {
  /**
   * Additional options for {@code systools:make_tar/2}.
   * 
   * @parameter
   */
  private String[] tarOptions;

  @Override
  public void execute() throws MojoExecutionException, MojoFailureException {
    Log log = getLog();
    Artifact artifact = this.project.getArtifact();
    String releaseName = getReleaseName(artifact);
    log.info("Packaging release of " + releaseName);
    StringBuilder command = new StringBuilder();
    command.append("Status = systools:make_tar(\"");
    command.append(releaseName);
    command.append("\"");
    command.append(", [");
    command.append("{outdir, \"");
    command.append(this.target.getPath());
    command.append("\"}, {dirs, [mibs, include");
    Collection<String> folders = getAdditionalFolders(this.targetLib);
    for (String folder : folders) {
      command.append(",");
      command.append(folder);
    }
    command.append("]}");
    if (this.tarOptions != null) {
      for (String option : this.tarOptions) {
        command.append(", ");
        command.append(option);
      }
    }
    command.append("]");
    command.append("), StatusCode = case Status of ok -> 0; _ -> 1 end, erlang:halt(StatusCode).");
    String result = eval(getLog(), command.toString(), getDependencies(this.targetLib), this.targetEbin);
    if (result != null && !result.isEmpty()) {
      log.info("make_tar returned " + result);
    }
    artifact.setFile(new File(this.target, releaseName + TARGZ_SUFFIX));
  }

  /**
   * Returns a {@link Collection} with additional folder to include in the
   * package .tar.gz. This will include all non-erlang source folders into the
   * package.  
   * 
   * @param root to start the scan in
   * @return a {@link Collection} with folders to include
   */
  private Collection<String> getAdditionalFolders(File root) {
    Set<String> folders = new HashSet<String>();
    for (File dir : getDirectoriesRecursive(this.targetLib, new FileFilter() {
      @Override
      public boolean accept(File dir) {
        return dir.getName().endsWith(SRC_SUFFIX);
      }
    })) {
      folders.add("\'" + dir.getName() + "\'");
    }
    return folders;
  }

  /**
   * Returns the release name for the given {@link Artifact}. The release name
   * consists of the artifacts id and its version. 
   * 
   * @param artifact to retrieve the release name from
   * @return a string containing the release name
   */
  private static String getReleaseName(Artifact artifact) {
    return artifact.getArtifactId() + "-" + artifact.getVersion();
  }
}
