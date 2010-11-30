package eu.lindenbaum.maven.cover;

import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Represents test coverage data for Erlang modules.
 * 
 * @author Olle Törnström <olle.toernstroem@lindenbaum.eu>
 */
public final class CoverData {
  private static final Pattern COVER_FUNCTION_REGEX = Pattern.compile("\\{\\{([^,}]+?),([^,}]+?),(\\d+)\\},\\{(\\d+),(\\d+)\\}\\}");
  private static final Pattern COVER_CLAUSE_REGEX = Pattern.compile("\\{\\{([^,}]+?),([^,}]+?),(\\d+),(\\d+)\\},\\{(\\d+),(\\d+)\\}\\}");
  private static final Pattern COVER_LINE_REGEX = Pattern.compile("\\{\\{([^,}]+?),(\\d+)\\},\\{(\\d+),(\\d+)\\}\\}");

  private final Map<String, ModuleCoverData> moduleCoverData;

  private int numberOfFunctions;
  private int numberOfClauses;
  private int numberOfLines;
  private int numberOfCoveredLines;
  private int numberOfNotCoveredLines;

  public CoverData(String coverageDataDump) {
    this.moduleCoverData = new HashMap<String, ModuleCoverData>();
    parseFunctionData(coverageDataDump);
    parseClauseData(coverageDataDump);
    parseLineData(coverageDataDump);
    calculateStatistics();
  }

  private void calculateStatistics() {
    for (ModuleCoverData module : this.moduleCoverData.values()) {
      module.calculateStatistics();
      this.numberOfFunctions += module.getNumberOfFunctions();
      this.numberOfClauses += module.getNumberOfClauses();
      this.numberOfLines += module.getNumberOfLines();
      this.numberOfCoveredLines += module.getNumberOfCoveredLines();
      this.numberOfNotCoveredLines += module.getNumberOfNotCoveredLines();
    }
  }

  private void parseFunctionData(String coverageDataDump) {
    Matcher functionMatcher = COVER_FUNCTION_REGEX.matcher(coverageDataDump);
    while (functionMatcher.find()) {
      String moduleName = functionMatcher.group(1);
      ModuleCoverData module = this.moduleCoverData.get(moduleName);
      if (module == null) {
        module = new ModuleCoverData(moduleName);
        this.moduleCoverData.put(moduleName, module);
      }
      String functionName = functionMatcher.group(2);
      int functionArity = Integer.parseInt(functionMatcher.group(3));
      String function = functionName + "/" + functionArity;
      int coveredLines = Integer.parseInt(functionMatcher.group(4));
      int notCoveredLines = Integer.parseInt(functionMatcher.group(5));
      CoverUnit coverUnit = new CoverUnit(coveredLines, notCoveredLines);
      module.putFunctionCoverData(function, coverUnit);
    }
  }

  private void parseClauseData(String inDump) {
    Matcher clauseMatcher = COVER_CLAUSE_REGEX.matcher(inDump);
    while (clauseMatcher.find()) {
      String moduleName = clauseMatcher.group(1);
      ModuleCoverData module = this.moduleCoverData.get(moduleName);
      String functionName = clauseMatcher.group(2);
      int functionArity = Integer.parseInt(clauseMatcher.group(3));
      String fun = functionName + "/" + functionArity;
      FunctionCoverData function = module.getFunctionCoverData(fun);
      int clauseIndex = Integer.parseInt(clauseMatcher.group(4));
      int coveredLines = Integer.parseInt(clauseMatcher.group(5));
      int notCoveredLines = Integer.parseInt(clauseMatcher.group(6));
      CoverUnit coverUnit = new CoverUnit(coveredLines, notCoveredLines);
      function.putClauseData(clauseIndex, coverUnit);
    }
  }

  private void parseLineData(String inDump) {
    Matcher lineMatcher = COVER_LINE_REGEX.matcher(inDump);
    while (lineMatcher.find()) {
      final String moduleName = lineMatcher.group(1);
      ModuleCoverData module = this.moduleCoverData.get(moduleName);
      int lineNumber = Integer.parseInt(lineMatcher.group(2));
      // TODO: Remove entries, line can only have one tuple!
      List<CoverUnit> entries = module.getLineCoverData(lineNumber);
      if (entries == null) {
        entries = new LinkedList<CoverUnit>();
        module.putLineCoverData(lineNumber, entries);
      }
      int coveredLines = Integer.parseInt(lineMatcher.group(3));
      int notCoveredLines = Integer.parseInt(lineMatcher.group(4));
      CoverUnit theCoverUnit = new CoverUnit(coveredLines, notCoveredLines);
      entries.add(theCoverUnit);
    }
  }

  public Collection<ModuleCoverData> getModuleCoverData() {
    return this.moduleCoverData.values();
  }

  public int getNumberOfModules() {
    return this.moduleCoverData.size();
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

  public boolean isCovered() {
    return this.numberOfNotCoveredLines == 0 && this.numberOfCoveredLines != 0;
  }
}
