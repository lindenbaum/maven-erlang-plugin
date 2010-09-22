package eu.lindenbaum.maven;

import static eu.lindenbaum.maven.util.FileUtils.APP_FILTER;
import static eu.lindenbaum.maven.util.FileUtils.NULL_FILTER;
import static eu.lindenbaum.maven.util.FileUtils.SNMP_FILTER;
import static eu.lindenbaum.maven.util.FileUtils.copyDirectory;

import java.io.File;
import java.io.FileFilter;
import java.util.HashMap;
import java.util.Map;

import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.project.MavenProject;

/**
 * Copy resources to target/priv.
 * 
 * @goal copy-resources
 * @phase generate-resources
 */
public final class CopyResourcesMojo extends AbstractMojo {
  /**
   * {@link MavenProject} to process.
   * 
   * @parameter expression="${project}"
   * @required
   * @readonly
   */
  private MavenProject project;

  /**
   * Directory to put the private resources into.
   * 
   * @parameter expression="${project.build.directory}/priv"
   * @required
   */
  private File privOutput;

  /**
   * Directory to put the application specific files into.
   * 
   * @parameter expression="${project.build.directory}/ebin"
   */
  private File ebinOutput;

  /**
   * Directory to put the SNMP specific filess into.
   * 
   * @parameter expression="${project.build.directory}/mibs/"
   */
  private File mibsOutput;

  /**
   * The base folder for sources of this project. This may be used to include sources from other languages
   * into the erlang application.
   * 
   * @parameter expression="${basedir}/src/main"
   */
  private File srcMain;

  /**
   * The erlang source folder of this project. It is supposed that the application related files (.app,
   * .appup, ...) and the SNMP related files (.mib, .funcs) reside here.
   * 
   * @parameter expression="${basedir}/src/main/erlang"
   */
  private File srcMainErlang;

  /**
   * First possible priv input directory.
   * 
   * @parameter expression="${basedir}/src/main/resources/priv"
   */
  private File privInput1;

  /**
   * Second possible priv input directory.
   * 
   * @parameter expression="${basedir}/src/main/priv"
   */
  private File privInput2;

  @Override
  public void execute() throws MojoExecutionException {
    Map<String, String> replacements = new HashMap<String, String>();
    replacements.put("?APP_VERSION", "\"" + this.project.getVersion() + "\"");

    int copiedFiles = 0;
    copiedFiles += copyDirectory(this.srcMain, this.privOutput, new FileFilter() {
      @Override
      public boolean accept(File file) {
        return file.isDirectory() && file.getName().equals("erlang");
      }
    });
    copiedFiles += copyDirectory(this.privInput1, this.privOutput, NULL_FILTER);
    copiedFiles += copyDirectory(this.privInput2, this.privOutput, NULL_FILTER);
    copiedFiles += copyDirectory(this.srcMainErlang, this.ebinOutput, APP_FILTER, replacements);
    copiedFiles += copyDirectory(this.srcMainErlang, this.mibsOutput, SNMP_FILTER);
    getLog().info("Copied " + copiedFiles + " files into " + this.privOutput.getAbsolutePath());
  }
}
