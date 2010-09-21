package eu.lindenbaum.maven;

import java.io.File;
import java.io.FileFilter;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import eu.lindenbaum.maven.util.ErlConstants;
import eu.lindenbaum.maven.util.FileUtils;

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;

/**
 * Compile the erlang sources.
 * 
 * @goal compile
 * @phase compile
 * @author Olivier Sambourg
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 */
public final class CompileMojo extends AbstractCompilerMojo {
  /**
   * Directory where the mibs files (.mib and .funcs) are copied.
   * 
   * @parameter expression="${project.build.directory}/mibs/"
   */
  private File mibsDirectory;

  /**
   * Directory where the mibs bin files are created.
   * 
   * @parameter expression="${project.build.directory}/priv/mibs"
   */
  private File mibsBinDirectory;

  /**
   * Directory where the beam files are created.
   * 
   * @parameter expression="${project.build.directory}/ebin/"
   */
  private File outputDirectory;

  /**
   * Source directory.
   * 
   * @parameter expression="${basedir}/src/main/erlang/"
   */
  private File inputDirectory;

  public void execute() throws MojoExecutionException, MojoFailureException {
    Map<String, String> replacements = new HashMap<String, String>();
    replacements.put("\\?APP_VERSION", "\"" + this.project.getVersion() + "\"");

    try {
      FileUtils.copyDirectory(this.inputDirectory, this.outputDirectory, new FileFilter() {
        @Override
        public boolean accept(File pathname) {
          if (pathname.isFile()) {
            String name = pathname.getName();
            return name.endsWith(ErlConstants.APP_SUFFIX) || name.endsWith(ErlConstants.APPUP_SUFFIX);
          }
          return true;
        }
      }, replacements);
      FileUtils.copyDirectory(this.inputDirectory, this.mibsDirectory, new FileFilter() {
        @Override
        public boolean accept(File pathname) {
          if (pathname.isFile()) {
            String name = pathname.getName();
            return name.endsWith(ErlConstants.MIB_SUFFIX) || name.endsWith(ErlConstants.FUNCS_SUFFIX);
          }
          return true;
        }
      });
    }
    catch (IOException e) {
      throw new MojoExecutionException(e.getMessage(), e);
    }

    int numFiles = 0;
    if (this.mibsDirectory.exists()) {
      numFiles += compileMibBin(this.mibsDirectory, this.mibsBinDirectory);
    }
    if (this.mibsBinDirectory.exists()) {
      numFiles += compileMibHrl(this.mibsBinDirectory, this.outputIncludeDirectory);
    }
    if (this.inputDirectory.exists()) {
      numFiles += compileSources(this.inputDirectory, this.outputDirectory, null, null);
    }
    if (numFiles == 0) {
      getLog().info("No sources to compile");
    }
  }
}
