package eu.lindenbaum.maven.report;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Locale;
import java.util.StringTokenizer;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import eu.lindenbaum.maven.ErlangReport;
import eu.lindenbaum.maven.Properties;
import eu.lindenbaum.maven.util.ErlConstants;
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
  private volatile Pattern modulesPattern;
  private volatile List<String[]> profilingResults = new ArrayList<String[]>();

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
  protected void execute(Log log, Locale l, Properties p) throws MojoExecutionException {

    List<File> profilingReports = FileUtils.getFilesRecursive(p.targetProfilingReports(), ".txt");
    if (profilingReports.size() < 1) {
      log.info("Nothing to do.");
      return;
    }

    parse(profilingReports.get(0), log, p);

    File outdir = new File(getReportOutputDirectory(), "profiling");
    FileUtils.ensureDirectories(outdir);

    generate(log, l);
  }

  void parse(File report, Log log, Properties p) throws MojoExecutionException {
    log.debug("Parsing profiling report file " + report.getName());
    makeModulesPattern(log, p);
    parseProfilingReportFile(report, log);
  }

  void makeModulesPattern(Log log, Properties p) {
    List<File> modules = FileUtils.getFilesRecursive(p.src(), ErlConstants.ERL_SUFFIX);
    StringBuilder patternString = new StringBuilder();
    boolean afterFirst = false;
    patternString.append("^(");
    for (File file : modules) {
      if (afterFirst) {
        patternString.append("|");
      }
      String moduleName = file.getName().replace(ErlConstants.ERL_SUFFIX, "");
      patternString.append(moduleName);
      afterFirst = true;
    }
    patternString.append("):.*$");
    this.modulesPattern = Pattern.compile(patternString.toString());
  }

  void parseProfilingReportFile(File report, Log log) throws MojoExecutionException {
    BufferedReader reader = null;
    FileReader file = null;
    try {
      file = new FileReader(report);
      reader = new BufferedReader(file);
      String line;
      while ((line = reader.readLine()) != null) {
        Matcher matcher = this.modulesPattern.matcher(line);
        if (matcher.matches()) {
          parseProfilingReportLine(line);
        }
      }
    }
    catch (FileNotFoundException e) {
      throw new MojoExecutionException("Unable to parse profiling file.", e);
    }
    catch (IOException e) {
      throw new MojoExecutionException("Unable to read profiling file.", e);
    }
    finally {
      if (reader != null) {
        try {
          reader.close();
        }
        catch (IOException e) {
        }
      }
      if (file != null) {
        try {
          file.close();
        }
        catch (IOException e) {
        }
      }
    }
  }

  /**
   * Lines to parse are always in the following well known format:
   * 
   * <pre>
   * FUNCTION     CALLS       %       TIME  [uS / CALLS]
   * mod:fun/2        1    0.00          0  [      0.00]
   * </pre>
   */
  void parseProfilingReportLine(String line) {
    StringTokenizer tokenizer = new StringTokenizer(line);
    String moduleFunctionArity = tokenizer.nextToken();
    String calls = tokenizer.nextToken();
    tokenizer.nextToken();
    String time = tokenizer.nextToken();
    tokenizer.nextToken();
    String microSecondsPerCall = tokenizer.nextToken().replace("]", "");
    this.profilingResults.add(new String[]{ moduleFunctionArity, calls, time, microSecondsPerCall });
  }

  void generate(Log log, Locale locale) {
    log.debug("Generating profling HTML report from " + this.profilingResults.size() + " result lines.");
    generateReportHeader(getSink(), locale);
    generateReportTable(getSink(), locale);
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
  void generateReportTable(Sink sink, Locale locale) {
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
    Collections.sort(this.profilingResults, new Comparator<String[]>() {
      @Override
      public int compare(String[] that, String[] other) {
        return Double.valueOf(that[3]).compareTo(Double.valueOf(other[3]));
      }
    });
    Collections.reverse(this.profilingResults);
    for (String[] values : this.profilingResults) {
      sink.tableRow();
      sinkCell(sink, values[0]);
      sinkCell(sink, values[1]);
      sinkCell(sink, values[2]);
      sinkCell(sink, values[3]);
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
}
