package eu.lindenbaum.maven;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.text.MessageFormat;
import java.util.List;
import java.util.Locale;

import eu.lindenbaum.maven.cover.CoverData;
import eu.lindenbaum.maven.cover.CoverUnit;
import eu.lindenbaum.maven.cover.FunctionCoverData;
import eu.lindenbaum.maven.cover.ModuleCoverData;
import eu.lindenbaum.maven.util.ErlConstants;
import eu.lindenbaum.maven.util.ErlUtils;

import org.apache.maven.doxia.sink.Sink;
import org.apache.maven.doxia.sink.SinkEventAttributeSet;
import org.apache.maven.doxia.siterenderer.Renderer;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.project.MavenProject;
import org.apache.maven.reporting.AbstractMavenReport;
import org.apache.maven.reporting.MavenReportException;

/**
 * Generates a report based on the results of the Maven invocations.
 * 
 * <b>Note:</b> This mojo doesn't fork any lifecycle, if you have a clean 
 * working copy, you have to use a command like <code>mvn clean integration-test
 * site</code> to ensure the build results are present when this goal is 
 * invoked.
 * 
 * @goal coverage-report
 * @execute phase="test" lifecycle="coverage-report"
 * 
 * @author Olle Törnström <olle.toernstroem@lindenbaum.eu>
 */
public class CoverageReport extends AbstractMavenReport {
  private static final String PATTERN_COVER_ANALYSE = "cover:import(\"{0}\"), io:write(lists:foldl(fun(Module, Acc) -> [lists:map(fun(Detail) -> cover:analyse(Module, coverage, Detail) end, [function, clause, line]) | Acc] end, [], cover:imported_modules())), io:nl().";

  /**
   * The output directory for the report. Note that this parameter is only 
   * evaluated if the goal is run directly from the command line. If the goal 
   * is run indirectly as part of a site generation, the output directory 
   * configured in the Maven Site Plugin is used instead.
   *
   * @parameter default-value="${project.reporting.outputDirectory}"
   * @required
   */
  protected File outputDirectory;

  /**
   * Directory where the beam files are created.
   * 
   * @parameter expression="${project.build.directory}/test"
   */
  private File testBeamDirectory;

  /**
   * @component
   */
  protected Renderer siteRenderer;

  /**
   * @parameter default-value="${project}"
   * @required
   * @readonly
   */
  protected MavenProject project;

  /**
   * @parameter expression="${basedir}/src/main/erlang/"
   * @readonly
   */
  private File sourceDirectory;

  private DecimalFormat percentFormat;

  /**
   * @throws MavenReportException  
   */
  @Override
  protected void executeReport(Locale locale) throws MavenReportException {
    try {
      DecimalFormatSymbols symbols = new DecimalFormatSymbols(locale);
      this.percentFormat = new DecimalFormat("0%", symbols);
      CoverData coverageData = getCoverageData();
      if (coverageData.getModuleCoverData().size() <= 0) {
        getLog().info("No coverage reports found, skipping report generation.");
        return;
      }
      Sink sink = getSink();
      constructHeader(sink);
      sink.body();
      constructHeadline(sink);
      constructSummary(coverageData, sink);
      constructModulesSummary(coverageData, sink);
      constructModulesList(coverageData, sink);
      sink.body_();
      sink.flush();
      sink.close();
    }
    catch (Throwable e) {
      getLog().error("Failed to generate report.", e);
    }
  }

  private CoverData getCoverageData() throws MojoExecutionException, MojoFailureException {
    File coverageDataFile = new File(this.testBeamDirectory, ErlConstants.COVERDATA_BIN);
    getLog().debug("Generating test coverage reports from " + coverageDataFile);
    String dumpCoverData = MessageFormat.format(PATTERN_COVER_ANALYSE, coverageDataFile.getPath());
    String coverageDump = ErlUtils.eval(getLog(), dumpCoverData);
    return new CoverData(coverageDump);
  }

  private void constructHeader(Sink sink) {
    sink.head();
    sink.title();
    sink.text("Erlang Test Coverage Report");
    sink.title_();
    sink.head_();
  }

  private void constructHeadline(Sink sink) {
    sink.section1();
    sink.sectionTitle1();
    sink.text("Erlang Test Coverage Report");
    sink.sectionTitle1_();
    sink.paragraph();
    sink.text("The results of the Erlang test coverage report.");
    sink.paragraph_();
    sink.section1_();
  }

  private void constructSummary(CoverData coverageData, Sink sink) {
    sink.section2();
    sink.sectionTitle2();
    sink.text("Summary");
    sink.sectionTitle2_();
    sink.section2_();
    sink.table();
    sink.tableRow();
    sinkTableHeader(sink, "");
    sinkTableHeader(sink, "Coverage");
    sinkTableHeader(sink, "Modules");
    sinkTableHeader(sink, "Functions");
    sinkTableHeader(sink, "Clauses");
    sinkTableHeader(sink, "Lines");
    sinkTableHeader(sink, "Covered lines");
    sinkTableHeader(sink, "Not covered lines");
    sink.tableRow_();
    sink.tableRow();
    int coveredLines = coverageData.getNumberOfCoveredLines();
    int notCoveredLines = coverageData.getNumberOfNotCoveredLines();
    sinkIsCoveredImageCell(sink, coverageData.isCovered());
    sinkCell(sink, this.percentFormat.format((double) coveredLines / (coveredLines + notCoveredLines)));
    sinkCell(sink, Integer.toString(coverageData.getNumberOfModules()));
    sinkCell(sink, Integer.toString(coverageData.getNumberOfFunctions()));
    sinkCell(sink, Integer.toString(coverageData.getNumberOfClauses()));
    sinkCell(sink, Integer.toString(coverageData.getNumberOfLines()));
    sinkCell(sink, Integer.toString(coveredLines));
    sinkCell(sink, Integer.toString(notCoveredLines));
    sink.tableRow_();
    sink.table_();
  }

  private void constructModulesSummary(CoverData coverageData, Sink sink) {
    sink.section2();
    sink.sectionTitle2();
    sink.text("Modules");
    sink.sectionTitle2_();
    sink.section2_();
    sink.table();
    sink.tableRow();
    sinkTableHeader(sink, "");
    sinkTableHeader(sink, "Coverage");
    sinkTableHeader(sink, "Module");
    sinkTableHeader(sink, "Functions");
    sinkTableHeader(sink, "Clauses");
    sinkTableHeader(sink, "Lines");
    sinkTableHeader(sink, "Covered lines");
    sinkTableHeader(sink, "Not covered lines");
    sink.tableRow_();
    for (ModuleCoverData module : coverageData.getModuleCoverData()) {
      sink.tableRow();
      sinkIsCoveredImageCell(sink, module.isCovered());
      int coveredLines = module.getNumberOfCoveredLines();
      int notCoveredLines = module.getNumberOfNotCoveredLines();
      sinkCell(sink, this.percentFormat.format((double) coveredLines / (coveredLines + notCoveredLines)));
      sinkModuleNameCell(sink, module);
      sinkCell(sink, Integer.toString(module.getNumberOfFunctions()));
      sinkCell(sink, Integer.toString(module.getNumberOfClauses()));
      sinkCell(sink, Integer.toString(module.getNumberOfLines()));
      sinkCell(sink, Integer.toString(coveredLines));
      sinkCell(sink, Integer.toString(notCoveredLines));
      sink.tableRow_();
    }
    sink.table_();
  }

  private void constructModulesList(CoverData coverageData, Sink sink) throws IOException {
    for (ModuleCoverData module : coverageData.getModuleCoverData()) {
      sink.section3();
      sink.sectionTitle3();
      sink.anchor(module.getModuleName());
      sink.text(module.getModuleName());
      sink.anchor_();
      sink.section3_();
      sink.table();
      sink.tableRow();
      sinkTableHeader(sink, "");
      sinkTableHeader(sink, "Coverage");
      sinkTableHeader(sink, "Function");
      sinkTableHeader(sink, "Clauses");
      sinkTableHeader(sink, "Lines");
      sinkTableHeader(sink, "Covered lines");
      sinkTableHeader(sink, "Not covered lines");
      sink.tableRow_();
      for (FunctionCoverData function : module.getFunctionCoverData()) {
        sink.tableRow();
        sinkIsCoveredImageCell(sink, function.isCovered());
        int coveredLines = function.getCoveredLines();
        int notCoveredLines = function.getNotCoveredLines();
        sinkCell(sink, this.percentFormat.format((double) coveredLines / (coveredLines + notCoveredLines)));
        sinkCell(sink, function.getFunctionName());
        sinkCell(sink, Integer.toString(function.getNumberOfClauses()));
        sinkCell(sink, Integer.toString(coveredLines + notCoveredLines));
        sinkCell(sink, Integer.toString(coveredLines));
        sinkCell(sink, Integer.toString(notCoveredLines));
        sink.tableRow_();
      }
      sink.table_();
      constructModuleLineCoverage(sink, module);
    }
  }

  private void constructModuleLineCoverage(Sink sink, ModuleCoverData module) throws IOException {
    File moduleSourceFile = new File(this.sourceDirectory, module.getModuleName() + ErlConstants.ERL_SUFFIX);
    FileReader fileReader = new FileReader(moduleSourceFile);
    BufferedReader reader = new BufferedReader(fileReader);
    sink.verbatim(SinkEventAttributeSet.BOXED);
    String line;
    int lineNumber = 1;
    while ((line = reader.readLine()) != null) {
      final String style;
      List<CoverUnit> lineCoverData = module.getLineCoverData(lineNumber);
      if (lineCoverData != null) {
        if (lineCoverData.get(0).isCovered()) {
          style = "style=\"background: #afa;\"";
        }
        else {
          style = "style=\"background: #faa;\"";
        }
      }
      else {
        style = "";
      }
      String pattern = "<span {0}><a name=\"{3}-{1}\">{1,number,0000}</a>: {2}</span>\n";
      String formattedLine = MessageFormat.format(pattern, style, lineNumber, line, module.getModuleName());
      sink.rawText(formattedLine);
      lineNumber++;
    }
    sink.verbatim_();
    reader.close();
  }

  private void sinkIsCoveredImageCell(Sink sink, boolean covered) {
    sinkImageCell(sink, covered ? "images/icon_success_sml.gif" : "images/icon_warning_sml.gif");
  }

  private void sinkModuleNameCell(Sink sink, ModuleCoverData module) {
    sink.tableCell();
    sink.link("#" + module.getModuleName());
    sink.text(module.getModuleName());
    sink.link_();
    sink.tableCell_();
  }

  private void sinkTableHeader(Sink sink, String header) {
    sink.tableHeaderCell();
    sink.text(header);
    sink.tableHeaderCell_();
  }

  private void sinkCell(Sink sink, String text) {
    sink.tableCell();
    sink.text(text);
    sink.tableCell_();
  }

  private void sinkImageCell(Sink sink, String string) {
    sink.tableCell();
    sink.figure();
    sink.figureGraphics(string);
    sink.figure_();
    sink.tableCell_();
  }

  @Override
  protected String getOutputDirectory() {
    return this.outputDirectory.getAbsolutePath();
  }

  @Override
  protected MavenProject getProject() {
    return this.project;
  }

  @Override
  protected Renderer getSiteRenderer() {
    return this.siteRenderer;
  }

  @Override
  public String getDescription(Locale locale) {
    return "Coverage results as reported by the Erlang coverage analysis tool.";
  }

  @Override
  public String getName(Locale locale) {
    return "Coverage Report";
  }

  @Override
  public String getOutputName() {
    return "erlang-coverage-report";
  }
}
