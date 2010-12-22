package eu.lindenbaum.maven.erlang;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

import eu.lindenbaum.maven.util.ErlUtils;

import org.apache.maven.plugin.logging.Log;

/**
 * Coverage report results for some project.
 * 
 * @author Olle Törnström <olle.toernstroem@lindenbaum.eu>
 */
public interface CoverageReportResult {

  /**
   * Log the coverage report output (e.g. infos/warnings/errors) using the
   * provided logger.
   * 
   * @param log used to print the output
   */
  public void logOutput(Log log);

  /**
   * Whether coverage generation failed, for some reason, or not.
   * 
   * @return {@code true} if coverage generation failed, {@code false} otherwise
   */
  public boolean failed();

  /**
   * Returns the coverage report results data, that can be used to generate
   * report output.
   * 
   * @return coverage report results data
   */
  public Report getReport();

  /**
   * Contains the coverage report data, available as a summary report for the
   * complete coverage report and each module and with a map of the modules that
   * were part of the coverage report.
   */
  public static class Report {
    private final Map<String, Module> modules = new HashMap<String, CoverageReportResult.Report.Module>();
    private int coverage;
    private int numberOfModules;
    private int numberOfFunctions;
    private int numberOfClauses;
    private int numberOfLines;
    private int numberOfCoveredLines;
    private int numberOfNotCoveredLines;

    /**
     * @param resultList to build the result data from
     */
    public Report(OtpErlangList resultList) {
      parseResultList(resultList);
      calculateNumberOfModules();
      calculateNumberOfFunctions();
      calculateNumberOfClauses();
      calculateNumberOfLines();
      calculateNumberOfCoveredLines();
      calculateNumberOfNotCoveredLines();
      calculateCoverage();
    }

    private void parseResultList(OtpErlangList resultList) {
      for (int i = 0; i < resultList.arity(); ++i) {
        OtpErlangTuple row = (OtpErlangTuple) resultList.elementAt(i);
        String type = ErlUtils.cast(row.elementAt(0));
        if ("module".equals(type)) {
          OtpErlangObject module = row.elementAt(1);
          OtpErlangObject covered = row.elementAt(2);
          OtpErlangObject notCovered = row.elementAt(3);
          add(new Module(module, covered, notCovered));
        }
        if ("function".equals(type)) {
          OtpErlangObject module = row.elementAt(1);
          OtpErlangObject function = row.elementAt(2);
          OtpErlangObject arity = row.elementAt(3);
          OtpErlangObject covered = row.elementAt(4);
          OtpErlangObject notCovered = row.elementAt(5);
          add(new Function(module, function, arity, covered, notCovered));
        }
        if ("clause".equals(type)) {
          OtpErlangObject module = row.elementAt(1);
          OtpErlangObject function = row.elementAt(2);
          OtpErlangObject arity = row.elementAt(3);
          OtpErlangObject index = row.elementAt(4);
          OtpErlangObject covered = row.elementAt(5);
          OtpErlangObject notCovered = row.elementAt(6);
          add(new Clause(module, function, arity, index, covered, notCovered));
        }
        if ("line".equals(type)) {
          OtpErlangObject module = row.elementAt(1);
          OtpErlangObject lineNumber = row.elementAt(2);
          OtpErlangObject covered = row.elementAt(3);
          OtpErlangObject notCovered = row.elementAt(4);
          add(new Line(module, lineNumber, covered, notCovered));
        }
      }
    }

    private void calculateCoverage() {
      this.coverage = calculateCoverage(this.numberOfCoveredLines, this.numberOfNotCoveredLines);
    }

    private void calculateNumberOfNotCoveredLines() {
      int numberOfNotCoveredLines1 = 0;
      for (Module m4 : this.modules.values()) {
        numberOfNotCoveredLines1 += m4.getNumberOfNotCoveredLines();
      }
      this.numberOfNotCoveredLines = numberOfNotCoveredLines1;
    }

    private void calculateNumberOfCoveredLines() {
      int numberOfCoveredLines1 = 0;
      for (Module m3 : this.modules.values()) {
        numberOfCoveredLines1 += m3.getNumberOfCoveredLines();
      }
      this.numberOfCoveredLines = numberOfCoveredLines1;
    }

    private void calculateNumberOfLines() {
      int numberOfLines1 = 0;
      for (Module m2 : this.modules.values()) {
        numberOfLines1 += m2.getNumberOfCoveredLines() + m2.getNumberOfNotCoveredLines();
      }
      this.numberOfLines = numberOfLines1;
    }

    private void calculateNumberOfClauses() {
      int numberOfClauses1 = 0;
      for (Module m1 : this.modules.values()) {
        numberOfClauses1 += m1.getNumberOfClauses();
      }
      this.numberOfClauses = numberOfClauses1;
    }

    private void calculateNumberOfFunctions() {
      int numberOfFunctions1 = 0;
      for (Module m : this.modules.values()) {
        numberOfFunctions1 += m.getNumberOfFunctions();
      }
      this.numberOfFunctions = numberOfFunctions1;
    }

    private void calculateNumberOfModules() {
      this.numberOfModules = this.modules.size();
    }

    void add(Module module) {
      this.modules.put(module.getName(), module);
    }

    void add(Function function) {
      this.modules.get(function.getModuleName()).add(function);
    }

    void add(Clause clause) {
      this.modules.get(clause.getModuleName()).add(clause);
    }

    void add(Line line) {
      this.modules.get(line.getModuleName()).add(line);
    }

    public int getNumberOfModules() {
      return this.numberOfModules;
    }

    public int getNumberOfFunctions() {
      return this.numberOfFunctions;
    }

    public int getNumberOfClauses() {
      return this.numberOfClauses;
    }

    public int getNumberOfLines() {
      return this.numberOfLines;
    }

    public int getNumberOfCoveredLines() {
      return this.numberOfCoveredLines;
    }

    public int getNumberOfNotCoveredLines() {
      return this.numberOfNotCoveredLines;
    }

    public int getCoverage() {
      return this.coverage;
    }

    public Collection<Module> getModules() {
      return this.modules.values();
    }

    public static final class Module {
      private final Map<String, Function> functions = new HashMap<String, CoverageReportResult.Report.Function>();
      private final Map<Integer, Boolean> lines = new HashMap<Integer, Boolean>();
      private final int coverage;
      private final String moduleName;
      private final int numberOfCoveredLines;
      private final int numberOfNotCoveredLines;

      public Module(OtpErlangObject name, OtpErlangObject covered, OtpErlangObject notCovered) {
        this.moduleName = ErlUtils.cast(name);
        this.numberOfCoveredLines = ErlUtils.toInt(covered);
        this.numberOfNotCoveredLines = ErlUtils.toInt(notCovered);
        this.coverage = calculateCoverage(this.numberOfCoveredLines, this.numberOfNotCoveredLines);
      }

      void add(Function function) {
        this.functions.put(function.getName(), function);
      }

      void add(Clause clause) {
        this.functions.get(clause.getFunctionName()).add(clause);
      }

      void add(Line line) {
        this.lines.put(line.getLineNumber(), line.isCovered());
      }

      public String getName() {
        return this.moduleName;
      }

      public int getNumberOfFunctions() {
        return this.functions.size();
      }

      public Collection<Function> getFunctions() {
        return this.functions.values();
      }

      public int getNumberOfClauses() {
        int numberOfClauses = 0;
        for (Function f : this.functions.values()) {
          numberOfClauses += f.getNumberOfClauses();
        }
        return numberOfClauses;
      }

      public int getNumberOfLines() {
        return this.numberOfCoveredLines + this.numberOfNotCoveredLines;
      }

      public int getNumberOfNotCoveredLines() {
        return this.numberOfNotCoveredLines;
      }

      public int getNumberOfCoveredLines() {
        return this.numberOfCoveredLines;
      }

      public int getCoverage() {
        return this.coverage;
      }

      public boolean notExecutable(int lineNumber) {
        return !this.lines.containsKey(lineNumber);
      }

      public boolean isCovered(int lineNumber) {
        return this.lines.get(lineNumber).booleanValue();
      }
    }

    public static final class Function {
      private final List<Clause> clauses = new ArrayList<CoverageReportResult.Report.Clause>();
      private final int coverage;
      private final String moduleName;
      private final String functionName;
      private final int numberOfCoveredLines;
      private final int numberOfNotCoveredLines;

      public Function(OtpErlangObject module,
                      OtpErlangObject name,
                      OtpErlangObject arity,
                      OtpErlangObject covered,
                      OtpErlangObject notCovered) {
        this.moduleName = ErlUtils.cast(module);
        this.functionName = ErlUtils.cast(name) + "/" + ErlUtils.toInt(arity);
        this.numberOfCoveredLines = ErlUtils.toInt(covered);
        this.numberOfNotCoveredLines = ErlUtils.toInt(notCovered);
        this.coverage = calculateCoverage(this.numberOfCoveredLines, this.numberOfNotCoveredLines);
      }

      void add(Clause clause) {
        this.clauses.add(clause);
      }

      public String getName() {
        return this.functionName;
      }

      public String getModuleName() {
        return this.moduleName;
      }

      public int getNumberOfClauses() {
        return this.clauses.size();
      }

      public int getCoverage() {
        return this.coverage;
      }

      public int getNumberOfCoveredLines() {
        return this.numberOfCoveredLines;
      }

      public int getNumberOfNotCoveredLines() {
        return this.numberOfNotCoveredLines;
      }

      public int getNumberOfLines() {
        return this.numberOfCoveredLines + this.numberOfNotCoveredLines;
      }
    }

    public static final class Clause {
      private final String moduleName;
      private final String functionName;

      @SuppressWarnings("unused")
      public Clause(OtpErlangObject module,
                    OtpErlangObject function,
                    OtpErlangObject arity,
                    OtpErlangObject index,
                    OtpErlangObject covered,
                    OtpErlangObject notCovered) {
        this.moduleName = ErlUtils.cast(module);
        this.functionName = ErlUtils.cast(function) + "/" + ErlUtils.toInt(arity);
      }

      public String getFunctionName() {
        return this.functionName;
      }

      public String getModuleName() {
        return this.moduleName;
      }
    }

    public static final class Line {
      private final String moduleName;
      private final int lineNumber;
      private final boolean isCovered;

      public Line(OtpErlangObject module,
                  OtpErlangObject lineNumber,
                  OtpErlangObject covered,
                  OtpErlangObject notCovered) {
        this.moduleName = ErlUtils.cast(module);
        this.lineNumber = ErlUtils.toInt(lineNumber);
        int cov = ErlUtils.toInt(covered);
        int not = ErlUtils.toInt(notCovered);
        this.isCovered = checkIfCovered(cov, not);
      }

      private boolean checkIfCovered(int cov, int not) {
        if (cov == 0 && not == 1) {
          return false;
        }
        else if (cov == 1 && not == 0) {
          return true;
        }
        else {
          throw new IllegalArgumentException("Bad coverage on line " + this.lineNumber + ": " + cov + ", "
                                             + not);
        }
      }

      public String getModuleName() {
        return this.moduleName;
      }

      public int getLineNumber() {
        return this.lineNumber;
      }

      public boolean isCovered() {
        return this.isCovered;
      }
    }

    /**
     * Calculates and return a coverage, always rounded down, to whole percent.
     */
    static final int calculateCoverage(int covered, int notCovered) {
      return (int) Math.floor(((double) covered / (covered + notCovered)) * 100);
    }
  }
}
