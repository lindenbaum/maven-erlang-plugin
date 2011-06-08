package eu.lindenbaum.maven.report;

import java.io.File;
import java.util.List;
import java.util.Locale;

import eu.lindenbaum.maven.ErlangReport;
import eu.lindenbaum.maven.Properties;
import eu.lindenbaum.maven.erlang.ProfilingResult;
import eu.lindenbaum.maven.erlang.ProfilingResult.Report;
import eu.lindenbaum.maven.erlang.ProfilingResult.Report.Row;
import eu.lindenbaum.maven.util.FileUtils;

import org.apache.maven.doxia.sink.Sink;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;

/**
 * Generates a profiling report from the output of an already executed
 * {@code profile} goal. NOTE: this report does not invoke any other lifecycle,
 * it is required that a profiling is already run before the report can be
 * generated.
 * 
 * @goal profiling-report
 * @author Olle Törnström <olle.toernstroem@lindenbaum.eu>
 * @since 2.1.0
 */
public class ProfilingReport extends ErlangReport {
  @Override
  public String getDescription(Locale l) {
    return "Profiling results, as reported by erlang:profiling analysis.";
  }

  @Override
  public String getName(Locale l) {
    return "profiling";
  }

  @Override
  public String getOutputName() {
    return "erlang-profiling-report";
  }

  @Override
  protected void execute(Log log, Locale locale, Properties p) throws MojoExecutionException {
    File profilingReportDir = p.targetLayout().profilingReports();
    List<File> profilingReports = FileUtils.getFilesRecursive(profilingReportDir, ".txt");
    if (profilingReports.size() < 1) {
      log.info("Nothing to do.");
      return;
    }
    Report report = new ProfilingResult.Report(profilingReports.get(0), p);
    File outdir = new File(getReportOutputDirectory(), "profiling");
    FileUtils.ensureDirectories(outdir);
    generate(report, log, locale);
    log.info("Successfully generated profiling report.");
  }

  void generate(Report report, Log log, Locale locale) {
    log.debug("Generating profling HTML report from " + report.getNumberOfRows() + " result lines.");
    generateReportHeader(getSink(), locale);
    generateReportTable(report, getSink(), locale);
    generateReportFooter(getSink(), locale);
  }

  void generateReportHeader(Sink sink, Locale locale) {
    String title = "Profiling Report";
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
  void generateReportTable(Report report, Sink sink, Locale locale) {
    sink.section2();
    sink.sectionTitle2();
    sink.text("Profiled functions");
    sink.sectionTitle2_();
    sink.section2_();
    sink.table();
    sink.tableRow();
    sinkTableHeader(sink, "Module:Function/Arity");
    sinkTableHeader(sink, "Calls");
    sinkTableHeader(sink, "Time");
    sinkTableHeader(sink, "µS/Call");
    sink.tableRow_();
    for (Row row : report.getRows()) {
      sink.tableRow();
      sinkCell(sink, row.name);
      sinkCell(sink, row.calls);
      sinkCell(sink, row.time);
      sinkCell(sink, row.microSecondsPerCall);
      sink.tableRow_();
    }
    sink.table_();
  }

  @SuppressWarnings("unused")
  void generateReportFooter(Sink sink, Locale locale) {
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

  private void sinkCell(Sink sink, float microSecondsPerCall) {
    sinkCell(sink, Float.toString(microSecondsPerCall));
  }

  private void sinkCell(Sink sink, int calls) {
    sinkCell(sink, Integer.toString(calls));
  }
}
