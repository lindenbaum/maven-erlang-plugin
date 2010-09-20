package eu.lindenbaum.maven;

import java.io.File;
import java.util.Locale;

import eu.lindenbaum.maven.util.ErlConstants;

import org.apache.maven.plugin.MojoExecutionException;

/**
 * Generate the edoc report.
 * 
 * @goal edoc
 * @phase generate-sources
 */
public final class EdocReportMojo extends AbstractEdocReportMojo {
  /**
   * Directory with module source code.
   * 
   * @parameter expression="${basedir}/src/main/erlang/"
   */
  private File sourceDirectory;

  /**
   * Destination directory.
   * 
   * @parameter default-value="${project.reporting.outputDirectory}/edoc"
   * @required
   */
  private File outputDirectory;

  /**
   * Name of the destination directory.
   * 
   * @parameter expression="${edocDestDir}" default-value="edoc"
   */
  private String edocDestDir;

  /**
   * Application resource file (in src/ directory).
   * 
   * @parameter
   */
  private String applicationResourceFile;

  /**
   * Edoc options.
   * 
   * @parameter
   */
  private String[] edocOptions;

  /*
   * MavenReport interface.
   */
  public String getName(Locale locale) {
    return "Edoc";
  }

  /*
   * MavenReport interface.
   */
  public String getDescription(Locale inLocale) {
    return "Edoc documentation";
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
    return this.edocDestDir + "/index";
  }

  /*
   * MavenReport interface.
   */
  @Override
  public boolean canGenerateReport() {
    return this.sourceDirectory.isDirectory() && this.sourceDirectory.list().length > 0;
  }

  /**
   * Actually generate the documentation.
   * 
   * @throws MojoExecutionException if a problem occurred with edoc.
   */
  @Override
  protected void generateEdocDocumentation() throws MojoExecutionException {
    this.outputDirectory.mkdirs();

    final String theApplicationName = ErlConstants.getApplicationName(this.applicationResourceFile,
                                                                  getProject().getArtifactId());
    final File theApplicationResourceFile = new File(this.sourceDirectory.getPath(), theApplicationName
                                                                                     + ".app");
    if (theApplicationResourceFile.exists()) {
      ErlConstants.generateEdocAppDocumentation(getLog(),
                                            getErlPath(),
                                            this.edocOptions,
                                            theApplicationName,
                                            this.sourceDirectory,
                                            this.sourceDirectory,
                                            this.outputDirectory);
    }
    else {
      ErlConstants.generateEdocFilesDocumentation(getLog(),
                                              getErlPath(),
                                              this.edocOptions,
                                              this.sourceDirectory,
                                              this.outputDirectory);
    }
  }
}
