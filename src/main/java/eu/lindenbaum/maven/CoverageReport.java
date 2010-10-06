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
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.reporting.MavenReportException;

/**
 * Generates a test coverage report with: project summary, showing the number of
 * functions, clauses executable lines and their test coverage percentage. A
 * module list with individual coverage reports and an extensive source code
 * report, with lines annotated in red or green, showing the exact coverage.
 * 
 * @goal coverage
 * @execute phase="test" lifecycle="coverage"
 * @author Olle Törnström <olle.toernstroem@lindenbaum.eu>
 */
public class CoverageReport extends AbstractErlangReport {
  private static final String PATTERN_COVER_ANALYSE = "cover:import(\"{0}\"), io:write(lists:foldl(fun(Module, Acc) -> [lists:map(fun(Detail) -> cover:analyse(Module, coverage, Detail) end, [function, clause, line]) | Acc] end, [], cover:imported_modules())), io:nl().";
  private static final String LINE_PATTERN = "<span {0}><a name=\"{3}-{1}\">{1,number,0000}</a>: {2}</span>\n";
  private static final String RED_LINE_ANNOTATION = "style=\"background: #faa;\"";
  private static final String GREEN_LINE_ANNOTATION = "style=\"background: #afa;\"";
  private static final String NO_LINE_ANNOTATION = "";
  private DecimalFormat percentFormat;
  volatile CoverData coverData;

  /**
   * @throws MavenReportException
   */
  @Override
  protected void executeReport(Locale locale) throws MavenReportException {
    try {
      loadCoverageData();
      if (canGenerateReport()) {
        initializeFormatting(locale);
        constructCoverageReport(locale);
      }
      else {
        getLog().info("No coverage data found, skipping report.");
      }
    }
    catch (Exception e) {
      throw new MavenReportException("Failed to generate coverage report.", e);
    }
  }

  void loadCoverageData() throws MojoExecutionException, MojoFailureException {
    if (this.coverData == null) {
      File coverDataFile = new File(this.targetTest, ErlConstants.COVERDATA_BIN);
      getLog().debug("Generating test coverage reports from " + coverDataFile);
      String dumpCoverData = MessageFormat.format(PATTERN_COVER_ANALYSE, coverDataFile.getPath());
      String coverageDump = ErlUtils.eval(getLog(), dumpCoverData);
      this.coverData = new CoverData(coverageDump);
    }
  }

  private void initializeFormatting(Locale locale) {
    DecimalFormatSymbols symbols = new DecimalFormatSymbols(locale);
    this.percentFormat = new DecimalFormat("0%", symbols);
  }

  private void constructCoverageReport(Locale locale) throws IOException {
    Sink sink = getSink();
    constructHeader(sink);
    sink.body();
    constructHeadline(sink, locale);
    constructSummary(this.coverData, sink);
    constructModulesSummary(this.coverData, sink);
    constructModulesList(this.coverData, sink);
    sink.body_();
    sink.flush();
    sink.close();
  }

  private void constructHeader(Sink sink) {
    sink.head();
    sink.title();
    sink.text("Erlang Test Coverage Report");
    sink.title_();
    sink.head_();
  }

  private void constructHeadline(Sink sink, Locale locale) {
    sink.section1();
    sink.sectionTitle1();
    sink.text("Erlang Test Coverage Report");
    sink.sectionTitle1_();
    sink.paragraph();
    sink.text(getDescription(locale));
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

  @SuppressWarnings("deprecation")
  void constructModuleLineCoverage(Sink sink, ModuleCoverData module) throws IOException {
    File moduleSourceFile = new File(this.srcMainErlang, module.getModuleName() + ErlConstants.ERL_SUFFIX);
    BufferedReader reader = new BufferedReader(new FileReader(moduleSourceFile));
    sink.verbatim(true);
    String sourceCodeLine;
    int lineNumber = 1;
    while ((sourceCodeLine = reader.readLine()) != null) {
      final String lineStyle = getLineStyle(module.getLineCoverData(lineNumber));
      String moduleName = module.getModuleName();
      String formattedLine = MessageFormat.format(LINE_PATTERN,
                                                  lineStyle,
                                                  lineNumber,
                                                  sourceCodeLine,
                                                  moduleName);
      sink.rawText(formattedLine);
      lineNumber++;
    }
    sink.verbatim_();
    reader.close();
  }

  private String getLineStyle(List<CoverUnit> lineCoverData) {
    final String style;
    if (lineCoverData != null) {
      if (lineCoverData.get(0).isCovered()) {
        style = GREEN_LINE_ANNOTATION;
      }
      else {
        style = RED_LINE_ANNOTATION;
      }
    }
    else {
      style = NO_LINE_ANNOTATION;
    }
    return style;
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
  public boolean canGenerateReport() {
    boolean canGenerateReport = false;
    try {
      loadCoverageData();
      if (this.coverData.getNumberOfModules() > 0) {
        canGenerateReport = true;
      }
    }
    catch (Throwable e) {
      getLog().error("Failed to check if report generation is possible, probably NOT then.", e);
    }
    return canGenerateReport;
  }

  @Override
  protected String getOutputDirectory() {
    return this.targetSiteCoverage.getAbsolutePath();
  }

  @Override
  public String getDescription(Locale locale) {
    return "Test coverage results, as reported by the Erlang coverage analysis tool.";
  }

  @Override
  public String getName(Locale locale) {
    return "Coverage";
  }

  @Override
  public String getOutputName() {
    return "erlang-coverage-report";
  }

  @Override
  public boolean isExternalReport() {
    return false;
  }
}
