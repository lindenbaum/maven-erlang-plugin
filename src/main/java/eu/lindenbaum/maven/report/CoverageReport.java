package eu.lindenbaum.maven.report;

import static eu.lindenbaum.maven.util.FileUtils.getFilesRecursive;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

import eu.lindenbaum.maven.ErlangReport;
import eu.lindenbaum.maven.Properties;
import eu.lindenbaum.maven.erlang.CoverageReportResult;
import eu.lindenbaum.maven.erlang.CoverageReportResult.Report;
import eu.lindenbaum.maven.erlang.CoverageReportResult.Report.Function;
import eu.lindenbaum.maven.erlang.CoverageReportResult.Report.Module;
import eu.lindenbaum.maven.erlang.CoverageReportScript;
import eu.lindenbaum.maven.erlang.LoadModulesScript;
import eu.lindenbaum.maven.erlang.MavenSelf;
import eu.lindenbaum.maven.erlang.PurgeModulesScript;
import eu.lindenbaum.maven.erlang.Script;
import eu.lindenbaum.maven.util.ErlConstants;
import eu.lindenbaum.maven.util.FileUtils;
import eu.lindenbaum.maven.util.MavenUtils;

import org.apache.maven.doxia.sink.Sink;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;

/**
 * Generates a test coverage report with: project summary, showing the number of
 * functions, clauses executable lines and their test coverage percentage. A
 * module list with individual coverage reports and an extensive source code
 * report, with lines annotated in red or green, showing the exact coverage.
 * <p>
 * ISSUE If a test purges or unloads a module to do coverage for, the coverage
 * compilation information will be gone and the coverage report will fail.
 * </p>
 * 
 * @goal coverage
 * @execute phase="test" lifecycle="coverage"
 * @author Olle Törnström <olle.toernstroem@lindenbaum.eu>
 */
public class CoverageReport extends ErlangReport {
  private static final String LINE_PATTERN = "<span {0}><a name=\"{3}-{1}\">{1,number,0000}</a>: {2}</span>\n";
  private static final String RED_LINE_ANNOTATION = "style=\"background: #faa;\"";
  private static final String GREEN_LINE_ANNOTATION = "style=\"background: #afa;\"";
  private static final String NO_LINE_ANNOTATION = "";

  @Override
  public String getDescription(Locale locale) {
    return "Test coverage results, as reported by the Erlang coverage analysis tool.";
  }

  @Override
  public String getName(Locale locale) {
    return "coverage";
  }

  @Override
  public String getOutputName() {
    return "erlang-coverage-report";
  }

  @Override
  public boolean canGenerateReport() {
    Properties p = getProperties();
    File targetTestEbin = p.targetTestEbin();
    return targetTestEbin.exists() && targetTestEbin.listFiles(FileUtils.BEAM_FILTER).length > 0;
  }

  @Override
  protected void execute(Log log, Locale locale, Properties p) throws MojoExecutionException {
    log.info(MavenUtils.SEPARATOR);
    log.info(" C O V E R A G E");
    log.info(MavenUtils.SEPARATOR);

    if (!canGenerateReport()) {
      log.info("Nothing to do.");
      return;
    }

    Script<Void> purgeScript = new PurgeModulesScript();
    MavenSelf.get(p.cookie()).exec(p.node(), purgeScript);

    File targetTestEbin = p.targetTestEbin();

    List<File> tests = new ArrayList<File>();
    tests.addAll(FileUtils.getFilesRecursive(p.targetTestEbin(), "_test" + ErlConstants.BEAM_SUFFIX));
    tests.addAll(FileUtils.getFilesRecursive(p.targetTestEbin(), "_tests" + ErlConstants.BEAM_SUFFIX));

    List<File> sources = getFilesRecursive(p.src(), ErlConstants.ERL_SUFFIX);

    List<File> testCodePaths = new ArrayList<File>();
    testCodePaths.add(p.targetTestEbin());
    testCodePaths.addAll(FileUtils.getDirectoriesRecursive(p.targetLib(), ErlConstants.BEAM_SUFFIX));
    testCodePaths.addAll(FileUtils.getDirectoriesRecursive(p.targetLib(), ErlConstants.HRL_SUFFIX));

    File outdir = new File(getReportOutputDirectory(), "coverage");
    outdir.mkdirs();

    Script<CoverageReportResult> script = new CoverageReportScript(targetTestEbin, tests, sources);
    CoverageReportResult result = MavenSelf.get(p.cookie()).eval(p.node(), script, testCodePaths);

    List<File> codePaths = FileUtils.getDirectoriesRecursive(p.targetLib(), ErlConstants.BEAM_SUFFIX);
    codePaths.add(p.targetEbin());
    List<File> modules = FileUtils.getFilesRecursive(p.targetLib(), ErlConstants.BEAM_SUFFIX);
    modules.addAll(FileUtils.getFilesRecursive(p.targetEbin(), ErlConstants.BEAM_SUFFIX));
    Script<Integer> loadScript = new LoadModulesScript(modules, codePaths);
    Integer loaded = MavenSelf.get(p.cookie()).exec(p.node(), loadScript);
    log.debug("Successfully reloaded " + loaded + " .beam file(s).");

    if (result.failed()) {
      throw new MojoExecutionException("failed to generate coverage report");
    }
    else {
      generateReportHeader(getSink(), locale, result.getReport());
      generateReportSummary(getSink(), locale, result.getReport());
      generateReportModulesSummary(getSink(), locale, result.getReport());
      generateReportForEachModule(getSink(), locale, result.getReport());
      generateReportFooter(getSink(), locale, result.getReport());

      log.info("Successfully generated coverage report.");
    }
  }

  @SuppressWarnings("unused")
  private void generateReportHeader(Sink sink, Locale locale, Report report) {
    String title = "Test Coverage Report";
    sink.head();
    sink.title();
    sink.text(title);
    sink.title_();
    sink.head_();
    sink.body();
    sink.section1();
    sink.sectionTitle1();
    sink.text(title);
    sink.sectionTitle1_();
    sink.paragraph();
    sink.text(getDescription(locale));
    sink.paragraph_();
    sink.section1_();
  }

  @SuppressWarnings("unused")
  private void generateReportSummary(Sink sink, Locale locale, Report report) {
    sink.section2();
    sink.sectionTitle2();
    sink.text("Summary");
    sink.sectionTitle2_();
    sink.section2_();
    sink.rawText("<!-- " + summaryComment(report) + " -->");
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
    sinkIsCoveredImageCell(sink, report.getCoverage() == 100);
    sinkCell(sink, report.getCoverage() + "%");
    sinkCell(sink, Integer.toString(report.getNumberOfModules()));
    sinkCell(sink, Integer.toString(report.getNumberOfFunctions()));
    sinkCell(sink, Integer.toString(report.getNumberOfClauses()));
    sinkCell(sink, Integer.toString(report.getNumberOfLines()));
    sinkCell(sink, Integer.toString(report.getNumberOfCoveredLines()));
    sinkCell(sink, Integer.toString(report.getNumberOfNotCoveredLines()));
    sink.tableRow_();
    sink.table_();
  }

  private String summaryComment(Report report) {
    StringBuilder sb = new StringBuilder();
    sb.append("summary: ").append(report.getCoverage());
    sb.append(",").append(report.getNumberOfModules());
    sb.append(",").append(report.getNumberOfFunctions());
    sb.append(",").append(report.getNumberOfClauses());
    sb.append(",").append(report.getNumberOfLines());
    sb.append(",").append(report.getNumberOfCoveredLines());
    sb.append(",").append(report.getNumberOfNotCoveredLines());
    return sb.toString();
  }

  @SuppressWarnings("unused")
  private void generateReportModulesSummary(Sink sink, Locale locale, Report report) {
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
    for (Module module : report.getModules()) {
      sink.tableRow();
      sinkIsCoveredImageCell(sink, module.getCoverage() == 100);
      sinkCell(sink, module.getCoverage() + "%");
      sinkModuleNameCell(sink, module);
      sinkCell(sink, Integer.toString(module.getNumberOfFunctions()));
      sinkCell(sink, Integer.toString(module.getNumberOfClauses()));
      sinkCell(sink, Integer.toString(module.getNumberOfLines()));
      sinkCell(sink, Integer.toString(module.getNumberOfCoveredLines()));
      sinkCell(sink, Integer.toString(module.getNumberOfNotCoveredLines()));
      sink.tableRow_();
    }
    sink.table_();
  }

  private void generateReportForEachModule(Sink sink, Locale locale, Report report) {
    for (Module module : report.getModules()) {
      sink.section3();
      sink.sectionTitle3();
      sink.anchor(module.getName());
      sink.text(module.getName());
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
      for (Function function : module.getFunctions()) {
        sink.tableRow();
        sinkIsCoveredImageCell(sink, function.getCoverage() == 100);
        sinkCell(sink, function.getCoverage() + "%");
        sinkCell(sink, function.getName());
        sinkCell(sink, Integer.toString(function.getNumberOfClauses()));
        sinkCell(sink, Integer.toString(function.getNumberOfLines()));
        sinkCell(sink, Integer.toString(function.getNumberOfCoveredLines()));
        sinkCell(sink, Integer.toString(function.getNumberOfNotCoveredLines()));
        sink.tableRow_();
      }
      sink.table_();
      generateModuleLineCoverage(sink, locale, module);
    }
  }

  @SuppressWarnings({ "unused", "deprecation" })
  private void generateModuleLineCoverage(Sink sink, Locale locale, Module module) {
    try {
      File moduleSourceFile = new File(getProperties().src(), module.getName() + ErlConstants.ERL_SUFFIX);
      BufferedReader reader;
      reader = new BufferedReader(new FileReader(moduleSourceFile));
      sink.verbatim(true);
      String sourceCodeLine;
      int lineNumber = 1;
      while ((sourceCodeLine = reader.readLine()) != null) {
        String lineStyle = lineStyle(module, lineNumber);
        String moduleName = module.getName();
        String cleanLine = sourceCodeLine.replaceAll("<", "&lt;").replaceAll(">", "&gt;");
        String formattedLine = MessageFormat.format(LINE_PATTERN,
                                                    lineStyle,
                                                    lineNumber,
                                                    cleanLine,
                                                    moduleName);
        sink.rawText(formattedLine);
        lineNumber++;
      }
      sink.verbatim_();
      reader.close();
    }
    catch (Exception e) {
      getLog().error(e.getMessage());
    }
  }

  private String lineStyle(Module module, int lineNumber) {
    if (module.notExecutable(lineNumber)) {
      return NO_LINE_ANNOTATION;
    }
    else {
      if (module.isCovered(lineNumber)) {
        return GREEN_LINE_ANNOTATION;
      }
      else {
        return RED_LINE_ANNOTATION;
      }
    }
  }

  @SuppressWarnings("unused")
  private void generateReportFooter(Sink sink, Locale locale, Report report) {
    sink.body_();
    sink.flush();
    sink.close();
  }

  // SINK UTILS ---------------------------------------------------------------

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

  private void sinkIsCoveredImageCell(Sink sink, boolean covered) {
    sinkImageCell(sink, covered ? "images/icon_success_sml.gif" : "images/icon_warning_sml.gif");
  }

  private void sinkImageCell(Sink sink, String string) {
    sink.tableCell();
    sink.figure();
    sink.figureGraphics(string);
    sink.figure_();
    sink.tableCell_();
  }

  private void sinkModuleNameCell(Sink sink, Module module) {
    sink.tableCell();
    sink.link("#" + module.getName());
    sink.text(module.getName());
    sink.link_();
    sink.tableCell_();
  }

}
