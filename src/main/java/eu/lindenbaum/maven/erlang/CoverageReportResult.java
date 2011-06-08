package eu.lindenbaum.maven.erlang;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.maven.plugin.MojoExecutionException;
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
     * @param reportFile to build the result data from
     * @throws MojoExecutionException 
     */
    public Report(File reportFile) throws MojoExecutionException {
      parseResultList(reportFile);
      calculateNumberOfModules();
      calculateNumberOfFunctions();
      calculateNumberOfClauses();
      calculateNumberOfLines();
      calculateNumberOfCoveredLines();
      calculateNumberOfNotCoveredLines();
      calculateCoverage();
    }

    private void parseResultList(File reportFile) throws MojoExecutionException {
      BufferedReader reader = null;
      FileReader file = null;
      try {
        file = new FileReader(reportFile);
        reader = new BufferedReader(file);
        String line;
        while ((line = reader.readLine()) != null) {
          String[] elements = line.split(" ");
          String type = elements[0];
          if ("module".equals(type)) {
            String module = elements[1];
            int covered = Integer.parseInt(elements[2]);
            int notCovered = Integer.parseInt(elements[3]);
            add(new Module(module, covered, notCovered));
          }
          if ("function".equals(type)) {
            String module = elements[1];
            String function = elements[2];
            int arity = Integer.parseInt(elements[3]);
            int covered = Integer.parseInt(elements[4]);
            int notCovered = Integer.parseInt(elements[5]);
            add(new Function(module, function, arity, covered, notCovered));
          }
          if ("clause".equals(type)) {
            String module = elements[1];
            String function = elements[2];
            int arity = Integer.parseInt(elements[3]);
            int index = Integer.parseInt(elements[4]);
            int covered = Integer.parseInt(elements[5]);
            int notCovered = Integer.parseInt(elements[6]);
            add(new Clause(module, function, arity, index, covered, notCovered));
          }
          if ("line".equals(type)) {
            String module = elements[1];
            int lineNumber = Integer.parseInt(elements[2]);
            int covered = Integer.parseInt(elements[3]);
            int notCovered = Integer.parseInt(elements[4]);
            add(new Line(module, lineNumber, covered, notCovered));
          }
        }
      }
      catch (FileNotFoundException e) {
        throw new MojoExecutionException("Unable to parse coverage file.", e);
      }
      catch (IOException e) {
        throw new MojoExecutionException("Unable to read coverage file.", e);
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

      /**
       * @param name of this module
       * @param covered lines
       * @param notCovered lines
       * @since 2.1.0
       */
      public Module(String name, int covered, int notCovered) {
        this.moduleName = name;
        this.numberOfCoveredLines = covered;
        this.numberOfNotCoveredLines = notCovered;
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

      /**
       * @param module name
       * @param name of this function
       * @param arity of this function
       * @param covered lines in this function
       * @param notCovered lines in this function
       * @since 2.1.0
       */
      public Function(String module, String name, int arity, int covered, int notCovered) {
        this.moduleName = module;
        this.functionName = name + "/" + arity;
        this.numberOfCoveredLines = covered;
        this.numberOfNotCoveredLines = notCovered;
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

      /**
       * @param module name
       * @param function name
       * @param arity of the function
       * @param index the clause in within the function
       * @param covered lines in this clause
       * @param notCovered lines in this clause
       * @since 2.1.0
       */
      @SuppressWarnings("unused")
      public Clause(String module, String function, int arity, int index, int covered, int notCovered) {
        this.moduleName = module;
        this.functionName = function + "/" + arity;
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

      /**
       * @param module name
       * @param lineNumber for this line
       * @param covered {@code 1} if this line is covered {@code 0} otherwise
       * @param notCovered {@code 1} if this line isn't covered {@code 0} otherwise
       * @since 2.1.0
       */
      public Line(String module, int lineNumber, int covered, int notCovered) {
        this.moduleName = module;
        this.lineNumber = lineNumber;
        this.isCovered = checkIfCovered(covered, notCovered);
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
      return (int) Math.floor((double) covered / (covered + notCovered) * 100);
    }
  }
}
