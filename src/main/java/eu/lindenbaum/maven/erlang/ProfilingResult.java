package eu.lindenbaum.maven.erlang;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.StringTokenizer;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import eu.lindenbaum.maven.Properties;
import eu.lindenbaum.maven.util.ErlConstants;
import eu.lindenbaum.maven.util.FileUtils;

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;

/**
 * Interface representing the result returned by the {@link ProfilingScript}.
 * 
 * @author Olle Törnström <olle.toernstroem@lindenbaum.eu>
 * @since 2.1.0
 */
public interface ProfilingResult {
  /**
   * Log the profiling test output (e.g. infos/warnings/errors) using the provided logger.
   * 
   * @param log used to print the output
   */
  void logOutput(Log log);

  /**
   * Returns whether all profiled tests passed, and that no test cases failed.
   * 
   * @return {@code true} if all tests passed, {@code false} otherwise.
   */
  boolean testsPassed();

  /**
   * Contains the profiling report row-data.
   */
  public static class Report {
    private volatile Pattern modulesPattern;
    private volatile List<Row> rows = new ArrayList<ProfilingResult.Report.Row>();

    /**
     * @param reportFile to build the result data from
     * @throws MojoExecutionException 
     */
    public Report(File reportFile, Properties p) throws MojoExecutionException {
      makeModulesPattern(p);
      parseProfilingReportFile(reportFile);
      Collections.sort(this.rows, new Comparator<Row>() {
        @Override
        public int compare(Row that, Row other) {
          return Double.compare(that.microSecondsPerCall, other.microSecondsPerCall);
        }
      });
      Collections.reverse(this.rows);
    }

    void makeModulesPattern(Properties p) {
      File src = p.sourceLayout().src();
      List<File> modules = FileUtils.getFilesRecursive(src, ErlConstants.ERL_SUFFIX);
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

    void parseProfilingReportFile(File report) throws MojoExecutionException {
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
            // ignored
          }
        }
        if (file != null) {
          try {
            file.close();
          }
          catch (IOException e) {
            // ignored
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
      int calls = Integer.parseInt(tokenizer.nextToken());
      tokenizer.nextToken();
      int time = Integer.parseInt(tokenizer.nextToken());
      tokenizer.nextToken();
      float microSecondsPerCall = Float.parseFloat(tokenizer.nextToken().replace("]", ""));
      this.rows.add(new Row(moduleFunctionArity, calls, time, microSecondsPerCall));
    }
    public static class Row {
      public final String name;
      public final int calls;
      public final int time;
      public final float microSecondsPerCall;

      public Row(String name, int calls, int time, float microsecondsPerCall) {
        this.name = name;
        this.calls = calls;
        this.time = time;
        this.microSecondsPerCall = microsecondsPerCall;
      }
    }

    public int getNumberOfRows() {
      return this.rows.size();
    }

    public Collection<Row> getRows() {
      return this.rows;
    }
  }
}
