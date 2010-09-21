package eu.lindenbaum.maven;

import static eu.lindenbaum.maven.util.ErlUtils.eval;
import static eu.lindenbaum.maven.util.FileUtils.getDependencies;

import java.io.File;

import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.project.MavenProject;

/**
 * Package the Erlang project.
 * 
 * @goal package-release
 * @phase package
 * @author Paul Guyot
 */
public final class PackageReleaseMojo extends AbstractMojo {
  /**
   * Project to interact with.
   * 
   * @parameter expression="${project}"
   * @required
   * @readonly
   */
  private MavenProject project;

  /**
   * Build directory.
   * 
   * @parameter expression="${project.build.directory}"
   * @readonly
   */
  private File buildDirectory;

  /**
   * Directories where dependencies are unpacked. This directory contains OTP applications (name-version
   * directories, with include and ebin sub directories).
   * 
   * @parameter expression="${project.build.directory}/lib/"
   * @required
   */
  private File libDirectory;

  /**
   * Bin directory, where binaries were created.
   * 
   * @parameter expression="${project.build.directory}/ebin"
   */
  private File binDirectory;

  /**
   * Project version.
   * 
   * @parameter expression="${project.version}"
   * @readonly
   */
  @SuppressWarnings("unused")
  private String version;

  /**
   * Name of the release. Defaults to the artifact id.
   * 
   * @parameter
   */
  private String releaseName;

  /**
   * Options for make_tar.
   * 
   * @parameter
   */
  private String[] makeTarOptions;

  public void execute() throws MojoExecutionException, MojoFailureException {
    final String theReleaseName;
    if (this.releaseName == null) {
      theReleaseName = this.project.getArtifactId().replace('-', '_');
    }
    else {
      theReleaseName = this.releaseName;
    }

    final StringBuilder theMakeTarLineBuffer = new StringBuilder();
    theMakeTarLineBuffer.append("Status = systools:make_tar(\"");
    final File theReleaseFile = new File(this.binDirectory, theReleaseName);
    theMakeTarLineBuffer.append(theReleaseFile.getPath());
    theMakeTarLineBuffer.append("\"");

    theMakeTarLineBuffer.append(", [");
    theMakeTarLineBuffer.append("{path, [\"").append(this.binDirectory.getPath()).append("\"]}, ");
    theMakeTarLineBuffer.append("{outdir, \"").append(this.buildDirectory.getPath()).append("\"}");
    if (this.makeTarOptions != null) {
      for (String theOption : this.makeTarOptions) {
        theMakeTarLineBuffer.append(", ");
        theMakeTarLineBuffer.append(theOption);
      }
    }
    theMakeTarLineBuffer.append("]");

    theMakeTarLineBuffer.append("), StatusCode = case Status of ok -> 0; _ -> 1 end, erlang:halt(StatusCode).");
    String theResult = eval(getLog(), theMakeTarLineBuffer.toString(), getDependencies(this.libDirectory));
    // Print any warning.
    if (!"".equals(theResult)) {
      getLog().info(theResult);
    }

    // Remark: we don't include separate files that may be generated, such as
    // separate packages for applications in a variable directory.
    // (see systools(3)).
    final File theOutputFile = new File(this.buildDirectory, theReleaseName + ".tar.gz");
    this.project.getArtifact().setFile(theOutputFile);
  }
}
