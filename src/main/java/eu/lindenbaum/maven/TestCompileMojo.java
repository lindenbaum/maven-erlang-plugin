package eu.lindenbaum.maven;

import java.io.File;
import java.io.FileFilter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import eu.lindenbaum.maven.util.ErlConstants;
import eu.lindenbaum.maven.util.FileUtils;

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;

/**
 * Compile the Erlang sources (with different options) and test cases.
 * 
 * @goal test-compile
 * @phase test
 * @requiresDependencyResolution compile
 * @author Olivier Sambourg
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 */
public final class TestCompileMojo extends AbstractCompilerMojo {
  /**
   * Source directory.
   * 
   * @parameter expression="${basedir}/src/main/erlang/"
   */
  protected File inputDirectory;

  /**
   * Directory where the beam files are created.
   * 
   * @parameter expression="${project.build.directory}/test"
   */
  private File outputDirectory;

  /**
   * Test sources directory.
   * 
   * @parameter expression="${basedir}/src/test/erlang/"
   */
  private File testDirectory;

  /**
   * Test include directory.
   * 
   * @parameter expression="${basedir}/src/test/include"
   */
  private File testIncludeDirectory;

  /**
   * Additional compilation options for tests.
   * 
   * @parameter
   */
  private String[] erlcTestOptions;

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
    }
    catch (IOException e) {
      throw new MojoExecutionException(e.getMessage(), e);
    }

    // Recompile with erlcTestOptions and +export_all.
    List<String> options = new ArrayList<String>();
    options.add("+export_all");
    if (this.erlcTestOptions != null) {
      options.addAll(Arrays.asList(this.erlcTestOptions));
    }

    int numFiles = 0;
    String[] opt = options.toArray(new String[0]);
    numFiles += compileSources(this.inputDirectory, this.outputDirectory, null, opt);
    numFiles += compileSources(this.testDirectory, this.outputDirectory, this.testIncludeDirectory, opt);
    if (numFiles == 0) {
      getLog().info("No sources to compile");
    }
  }
}
