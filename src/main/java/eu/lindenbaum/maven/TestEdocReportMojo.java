package eu.lindenbaum.maven;

import java.io.File;
import java.util.Locale;

import eu.lindenbaum.maven.util.ErlConstants;

import org.apache.maven.plugin.MojoExecutionException;

/**
 * Generate the edoc report for test source.
 * 
 * @goal test-edoc
 * @phase generate-test-sources
 */
public final class TestEdocReportMojo extends AbstractEdocReportMojo {
  /**
   * Directory with module test source code.
   * 
   * @parameter expression="${basedir}/src/test/erlang/"
   */
  private File testSourceDirectory;

  /**
   * Destination directory.
   * 
   * @parameter default-value="${project.reporting.outputDirectory}/test-edoc"
   * @required
   */
  private File outputDirectory;

  /**
   * Name of the destination directory.
   * 
   * @parameter expression="${testEdocDestDir}" default-value="test-edoc"
   */
  private String testEdocDestDir;

  /**
   * Edoc options.
   * 
   * @parameter
   */
  private String[] testEdocOptions;

  /*
   * MavenReport interface.
   */
  public String getName(Locale locale) {
    return "Edoc (Tests)";
  }

  /*
   * MavenReport interface.
   */
  public String getDescription(Locale inLocale) {
    return "Edoc documentation for tests";
  }

  /*
   * MavenReport interface.
   */
  @Override
  protected String getOutputDirectory() {
    return this.outputDirectory.getAbsoluteFile().toString();
  }

  /*
   * MavenReport Interface.
   */
  public String getOutputName() {
    return this.testEdocDestDir + "/index";
  }

  /*
   * MavenReport interface.
   */
  @Override
  public boolean canGenerateReport() {
    return this.testSourceDirectory.isDirectory() && this.testSourceDirectory.list().length > 0;
  }

  /**
   * Actually generate the documentation.
   * 
   * @throws MojoExecutionException if a problem occurred with edoc.
   */
  @Override
  protected void generateEdocDocumentation() throws MojoExecutionException {
    this.outputDirectory.mkdirs();

    ErlConstants.generateEdocFilesDocumentation(getLog(),
                                                this.testEdocOptions,
                                                this.testSourceDirectory,
                                                this.outputDirectory);
  }
}
