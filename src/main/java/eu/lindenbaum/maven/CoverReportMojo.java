package eu.lindenbaum.maven;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;

import javax.xml.transform.Templates;
import javax.xml.transform.TransformerException;

import eu.lindenbaum.maven.cover.CoverData;
import eu.lindenbaum.maven.cover.ModuleCoverData;
import eu.lindenbaum.maven.util.ErlUtils;

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.project.MavenProject;
import org.apache.maven.reporting.AbstractMavenReport;
import org.apache.maven.reporting.MavenReportException;
import org.codehaus.doxia.site.renderer.SiteRenderer;

/**
 * Generate the cover report.
 * 
 * @goal cover-report
 * @execute phase="test" lifecycle="cover-report"
 */
public class CoverReportMojo extends AbstractMavenReport {
  /**
   * Erlang code to dump the cover data.
   */
  private static final String DUMP_COVERDATA = "io:write(lists:foldl(fun(Module, Acc) -> [lists:map(fun(Detail) -> cover:analyse(Module, coverage, Detail) end, [function, clause, line]) | Acc] end, [], cover:imported_modules())), io:nl().";

  /**
   * XML Suffix.
   */
  private static final String XML_SUFFIX = ".xml";

  /**
   * Directory with the modules files.
   */
  private static final String MODULES_SUBDIR = "cover-reports";

  /**
   * Path of the default stylesheet, as available within the jar.
   */
  private static final String DEFAULT_STYLESHEET = "org/erlang/maven/cover.xsl";

  /**
   * The stylesheet for the transformation. If not present, the plug-in will use the default stylesheet.
   * 
   * @parameter
   */
  private File xslStylesheet;

  /**
   * Directory with module source code.
   * 
   * @parameter expression="${basedir}/src/main/erlang/"
   */
  private File sourceDirectory;

  /**
   * Directory with test module source code.
   * 
   * @parameter expression="${basedir}/src/test/erlang/"
   */
  private File testSourceDirectory;

  /**
   * The output directory for the report.
   * 
   * @parameter default-value="${project.reporting.outputDirectory}"
   * @required
   */
  private File outputDirectory;

  /**
   * Doxia Site Renderer.
   * 
   * @parameter expression="${component.org.codehaus.doxia.site.renderer.SiteRenderer}"
   * @required @readonly
   */
  private SiteRenderer siteRenderer;

  /**
   * Project to interact with.
   * 
   * @parameter expression="${project}"
   * @required
   * @readonly
   */
  private MavenProject project;

  /**
   * Path to the erlang installation directory.
   * 
   * @parameter
   */
  private String erlPath;

  /**
   * Directory where the beam files are created.
   * 
   * @parameter expression="${project.build.directory}/test"
   */
  private File testBeamDirectory;

  /**
   * The filename to use for the report.
   * 
   * @parameter expression="${outputName}" default-value="cover-report"
   * @required
   */
  private String outputName;

  public String getName(Locale locale) {
    return "Erlang Test Coverage";
  }

  public String getDescription(Locale inLocale) {
    return "Erlang Coverage Report.";
  }

  /*
   * MavenReport interface.
   */
  @Override
  protected String getOutputDirectory() {
    return this.outputDirectory.getAbsolutePath();
  }

  /*
   * MavenReport interface.
   */
  @Override
  protected MavenProject getProject() {
    return this.project;
  }

  /*
   * MavenReport interface.
   */
  @Override
  protected SiteRenderer getSiteRenderer() {
    return this.siteRenderer;
  }

  /*
   * MavenReport interface.
   */
  @Override
  protected void executeReport(Locale locale) throws MavenReportException {
    //    if (!canGenerateReport()) {
    //      return;
    //    }
    //    final File theCoverageDataFile = new File(this.testBeamDirectory, ErlUtils.COVERDATA_BIN);
    //    try {
    //      getLog().debug("Generating XML reports in " + this.outputDirectory);
    //      this.outputDirectory.mkdirs();
    //
    //      final List<File> theReports = generateXMLReports(theCoverageDataFile);
    //
    //      getLog().debug("Compiling the XSLT.");
    //      // Prepare the stylesheet.
    //      final TransformerFactoryImpl factory = new TransformerFactoryImpl();
    //      final Configuration theConfig = factory.getConfiguration();
    //      theConfig.setErrorListener(new ErrorListener() {
    //        public void error(TransformerException exception) throws TransformerException {
    //          getLog().error(exception.getMessage(), exception);
    //          throw exception;
    //        }
    //
    //        public void fatalError(TransformerException exception) throws TransformerException {
    //          getLog().error(exception.getMessage(), exception);
    //          throw exception;
    //        }
    //
    //        public void warning(TransformerException exception) throws TransformerException {
    //          getLog().warn(exception.getMessage(), exception);
    //        }
    //      });
    //      final Source theTemplateStream;
    //      if (this.xslStylesheet == null) {
    //        final InputStream theCompiledStylesheetStream = getClass().getClassLoader()
    //                                                                  .getResourceAsStream(DEFAULT_STYLESHEET);
    //        if (theCompiledStylesheetStream == null) {
    //          throw new MavenReportException("Could not find stylesheet " + DEFAULT_STYLESHEET);
    //        }
    //        theTemplateStream = new StreamSource(theCompiledStylesheetStream);
    //      }
    //      else {
    //        theTemplateStream = new StreamSource(this.xslStylesheet);
    //      }
    //      final Templates theStylesheet = null;//factory.newTemplates(theTemplateStream);
    //
    //      getLog().debug("Generating HTML reports.");
    //      for (File theReport : theReports) {
    //        final File theXMLReportDir = theReport.getParentFile();
    //        final String theRootDir;
    //        if (theXMLReportDir.equals(this.outputDirectory)) {
    //          theRootDir = "./";
    //        }
    //        else {
    //          theRootDir = "../";
    //        }
    //        generateHTMLReport(theRootDir, theStylesheet, theReport);
    //      }
    //    }
    //    catch (MojoExecutionException anException) {
    //      throw new MavenReportException(anException.getMessage(), anException);
    //    }
    //    catch (IOException anException) {
    //      throw new MavenReportException(anException.getMessage(), anException);
    //    }
    //    catch (TransformerException anException) {
    //      throw new MavenReportException(anException.getMessage(), anException);
    //    }
  }

  public String getOutputName() {
    return this.outputName;
  }

  /*
   * MavenReport interface.
   */
  @Override
  public boolean isExternalReport() {
    return true;
  }

  /*
   * MavenReport interface.
   */
  @Override
  public boolean canGenerateReport() {
    final File theCoverageDataFile = new File(this.testBeamDirectory, ErlUtils.COVERDATA_BIN);
    if (!theCoverageDataFile.exists()) {
      getLog().debug("Cover data file (" + theCoverageDataFile + ") doesn't exist");
      return false;
    }

    return true;
  }

  /**
   * Generate the XML reports and return the list of module files.
   * 
   * @param inCoverageDataFile data file generated by cover.
   * @return the list of XML reports.
   * @throws MojoExecutionException if a problem occus while generating the report.
   * @throws IOException if the reports cannot be written.
   */
  private List<File> generateXMLReports(File inCoverageDataFile) throws MojoExecutionException, IOException {
    getLog().debug("Generating XML reports from " + inCoverageDataFile);

    final List<File> theResult = new LinkedList<File>();

    final String theCoverageDump = ErlUtils.eval(getLog(), this.erlPath, "cover:import(\""
                                                                         + inCoverageDataFile.getPath()
                                                                         + "\"), " + DUMP_COVERDATA);
    final CoverData theCoverData = new CoverData(theCoverageDump);

    theResult.add(generateMainXMLReport(theCoverData));

    // Write an XML file for each module.
    final File theModulesXMLDirectory = new File(this.outputDirectory, MODULES_SUBDIR);
    theModulesXMLDirectory.mkdirs();
    for (ModuleCoverData theModule : theCoverData.getModuleCoverData()) {
      getLog().debug("Generating XML report for module " + theModule.getModuleName());
      theResult.add(generateModuleXMLReport(theModulesXMLDirectory, theModule));
    }
    return theResult;
  }

  /**
   * Generate the main XML report.
   * 
   * @param inCoverData coverage data.
   * @return the main XML report.
   * @throws IOException if the report cannot be written.
   */
  private File generateMainXMLReport(CoverData inCoverData) throws IOException {
    // Write the cover.xml file.
    final File theCoverageDataXMLFile = new File(this.outputDirectory, this.outputName + XML_SUFFIX);
    final Writer theWriter = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(theCoverageDataXMLFile),
                                                                       "UTF-8"));
    inCoverData.writeToXMLFile(theWriter);
    theWriter.close();

    return theCoverageDataXMLFile;
  }

  /**
   * Generate the main XML report.
   * 
   * @param inModuleXMLDirectory directory where to create the XML file.
   * @param inModule coverage data for the module.
   * @return the main XML report.
   * @throws IOException if the report cannot be written.
   */
  private File generateModuleXMLReport(File inModuleXMLDirectory, ModuleCoverData inModule) throws IOException {
    // Write the cover.xml file.
    final String theModuleName = inModule.getModuleName();
    File theSourceFile = new File(this.sourceDirectory, theModuleName + ErlUtils.ERL_SUFFIX);
    if (!theSourceFile.exists()) {
      theSourceFile = new File(this.testSourceDirectory, theModuleName + ErlUtils.ERL_SUFFIX);
      if (!theSourceFile.exists()) {
        getLog().info("Could not find source for module " + theModuleName);
        theSourceFile = null;
      }
    }
    final File theModuleXMLFile = new File(inModuleXMLDirectory, "module-" + theModuleName + XML_SUFFIX);
    final Writer theModuleWriter = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(theModuleXMLFile),
                                                                             "UTF-8"));
    inModule.writeToXMLFile(theModuleWriter, theSourceFile);
    theModuleWriter.close();

    return theModuleXMLFile;
  }

  /**
   * Generate the HTML report from an XML report.
   * 
   * @param inRootDir value for the rootdir XLST parameter.
   * @param inStylesheet stylesheet for the transformation.
   * @param inXMLReport XML file to transform.
   * @throws TransformerException if there was a problem with the XSLT transformation.
   */
  private void generateHTMLReport(String inRootDir, Templates inStylesheet, File inXMLReport) throws TransformerException {
    //    final Source theXMLSource = new StreamSource(inXMLReport);
    //    final Transform theTransform = new Transform();
    //    final ArrayList<String> theParams = new ArrayList<String>();
    //    theParams.add("rootdir=" + inRootDir);
    //    final String theXMLName = inXMLReport.getName();
    //    final int theXMLNameLen = theXMLName.length();
    //    final String theHTMLName;
    //    if (theXMLName.endsWith(".xml")) {
    //      theHTMLName = theXMLName.substring(0, theXMLNameLen - 4) + ".html";
    //    }
    //    else {
    //      theHTMLName = theXMLNameLen + ".html";
    //    }
    //    final File theHTMLFile = new File(inXMLReport.getParent(), theHTMLName);
    //    theTransform.processFile(theXMLSource, inStylesheet, theHTMLFile, theParams, null);
  }
}
