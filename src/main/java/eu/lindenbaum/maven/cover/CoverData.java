package eu.lindenbaum.maven.cover;

import java.io.IOException;
import java.io.Writer;
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
  private boolean isCovered;

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
      this.isCovered &= module.isCovered();
    }
  }

  private void parseFunctionData(String coverageDataDump) {
    Matcher functionMatcher = COVER_FUNCTION_REGEX.matcher(coverageDataDump);
    while (functionMatcher.find()) {
      String theModuleName = functionMatcher.group(1);
      ModuleCoverData theModuleData = this.moduleCoverData.get(theModuleName);
      if (theModuleData == null) {
        theModuleData = new ModuleCoverData(theModuleName);
        this.moduleCoverData.put(theModuleName, theModuleData);
      }
      String theFunctionName = functionMatcher.group(2);
      int theFunctionArity = Integer.parseInt(functionMatcher.group(3));
      String theFunction = theFunctionName + "/" + theFunctionArity;
      int theCoveredLines = Integer.parseInt(functionMatcher.group(4));
      int theNotCoveredLines = Integer.parseInt(functionMatcher.group(5));
      CoverUnit theCoverUnit = new CoverUnit(theCoveredLines, theNotCoveredLines);
      theModuleData.putFunctionCoverData(theFunction, theCoverUnit);
    }
  }

  private void parseClauseData(String inDump) {
    final Matcher clauseMatcher = COVER_CLAUSE_REGEX.matcher(inDump);
    while (clauseMatcher.find()) {
      final String theModuleName = clauseMatcher.group(1);
      ModuleCoverData theModuleData = this.moduleCoverData.get(theModuleName);
      if (theModuleData == null) {
        theModuleData = new ModuleCoverData(theModuleName);
        this.moduleCoverData.put(theModuleName, theModuleData);
      }
      final String theFunctionName = clauseMatcher.group(2);
      final int theFunctionArity = Integer.parseInt(clauseMatcher.group(3));
      final String theFunction = theFunctionName + "/" + theFunctionArity;
      final FunctionCoverData theOriginalFunctionData = theModuleData.getFunctionCoverData(theFunction);
      final FunctionCoverData theFunctionData;
      if (theOriginalFunctionData == null) {
        theFunctionData = new FunctionCoverData(theFunctionName);
        theModuleData.putFunctionCoverData(theFunction, theFunctionData);
      }
      else {
        theFunctionData = theOriginalFunctionData;
      }
      final int theClauseIndex = Integer.parseInt(clauseMatcher.group(4));
      final int theCoveredLines = Integer.parseInt(clauseMatcher.group(5));
      final int theNotCoveredLines = Integer.parseInt(clauseMatcher.group(6));
      final CoverUnit theCoverUnit = new CoverUnit(theCoveredLines, theNotCoveredLines);
      theFunctionData.putClauseData(theClauseIndex, theCoverUnit);
    }
  }

  private void parseLineData(String inDump) {
    final Matcher lineMatcher = COVER_LINE_REGEX.matcher(inDump);
    while (lineMatcher.find()) {
      final String theModuleName = lineMatcher.group(1);
      ModuleCoverData theModuleData = this.moduleCoverData.get(theModuleName);
      if (theModuleData == null) {
        theModuleData = new ModuleCoverData(theModuleName);
        this.moduleCoverData.put(theModuleName, theModuleData);
      }
      final int theLine = Integer.parseInt(lineMatcher.group(2));
      List<CoverUnit> theList = theModuleData.getLineCoverData(theLine);
      if (theList == null) {
        theList = new LinkedList<CoverUnit>();
        theModuleData.putLineCoverData(theLine, theList);
      }
      final int theCoveredLines = Integer.parseInt(lineMatcher.group(3));
      final int theNotCoveredLines = Integer.parseInt(lineMatcher.group(4));
      final CoverUnit theCoverUnit = new CoverUnit(theCoveredLines, theNotCoveredLines);
      theList.add(theCoverUnit);
    }
  }

  /**
   * Accessor on the module data.
     *
     * @return the set of module data.
   */
  public Collection<ModuleCoverData> getModuleCoverData() {
    return this.moduleCoverData.values();
  }

  /**
   * Write the data to an XML file.
   * This function writes the XML header.
   *
   * @param inXMLFile     writer to write to.
   * @throws IOException if there was a problem while writing the file.
   * 
   * @deprecated Not to be used, will be removed.
   */
  @Deprecated
  public void writeToXMLFile(Writer inXMLFile) throws IOException {
    inXMLFile.write("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");
    inXMLFile.write("<coverdata>\n");
    if (!this.moduleCoverData.isEmpty()) {
      inXMLFile.write("  <modules>\n");
      for (ModuleCoverData theModule : this.moduleCoverData.values()) {
        theModule.writeToXMLFile(inXMLFile, "    ");
      }
      inXMLFile.write("  </modules>\n");
    }
    inXMLFile.write("</coverdata>\n");
  }

  /**
   * @deprecated Not to be used, will be removed.
   */
  @Deprecated
  static String escapeXml(String inText) {
    return inText.replaceAll("&", "&amp;")
                 .replaceAll("\"", "&quot;")
                 .replaceAll("<", "&lt;")
                 .replaceAll(">", "&gt;");
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
    return this.isCovered;
  }
}
