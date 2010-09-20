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
 * Class for handling cover data.
 */
public final class CoverData {
  /**
   * Regex to scan function information from the cover data.
   */
  private static final Pattern COVER_FUNCTION_REGEX = Pattern.compile("\\{\\{([^,}]+?),([^,}]+?),(\\d+)\\},\\{(\\d+),(\\d+)\\}\\}");

  /**
   * Regex to scan clause information from the cover data.
   */
  private static final Pattern COVER_CLAUSE_REGEX = Pattern.compile("\\{\\{([^,}]+?),([^,}]+?),(\\d+),(\\d+)\\},\\{(\\d+),(\\d+)\\}\\}");

  /**
   * Regex to scan line information from the cover data.
   */
  private static final Pattern COVER_LINE_REGEX = Pattern.compile("\\{\\{([^,}]+?),(\\d+)\\},\\{(\\d+),(\\d+)\\}\\}");

  /**
   * Reference on the module cover data.
   */
  private final Map<String, ModuleCoverData> mModuleCoverData;

  /**
   * Constructor from a dumped cover data.
   *
   * @param inDump    output of the cover dump.
   */
  public CoverData(String inDump) {
    this.mModuleCoverData = new HashMap<String, ModuleCoverData>();
    parseFunctionData(inDump);
    parseClauseData(inDump);
    parseLineData(inDump);
  }

  /**
   * Parse the function data from the dump.
   *
   * @param inDump    output of the cover dump.
   */
  public void parseFunctionData(String inDump) {
    final Matcher functionMatcher = COVER_FUNCTION_REGEX.matcher(inDump);
    while (functionMatcher.find()) {
      final String theModuleName = functionMatcher.group(1);

      ModuleCoverData theModuleData = this.mModuleCoverData.get(theModuleName);
      if (theModuleData == null) {
        theModuleData = new ModuleCoverData(theModuleName);
        this.mModuleCoverData.put(theModuleName, theModuleData);
      }

      final String theFunctionName = functionMatcher.group(2);
      final int theFunctionArity = Integer.parseInt(functionMatcher.group(3));
      final String theFunction = theFunctionName + "/" + theFunctionArity;
      final int theCoveredLines = Integer.parseInt(functionMatcher.group(4));
      final int theNotCoveredLines = Integer.parseInt(functionMatcher.group(5));
      final CoverUnit theCoverUnit = new CoverUnit(theCoveredLines, theNotCoveredLines);
      theModuleData.putFunctionCoverData(theFunction, theCoverUnit);
    }
  }

  /**
   * Parse the clause data from the dump.
   *
   * @param inDump    output of the cover dump.
   */
  public void parseClauseData(String inDump) {
    final Matcher clauseMatcher = COVER_CLAUSE_REGEX.matcher(inDump);
    while (clauseMatcher.find()) {
      final String theModuleName = clauseMatcher.group(1);

      ModuleCoverData theModuleData = this.mModuleCoverData.get(theModuleName);
      if (theModuleData == null) {
        theModuleData = new ModuleCoverData(theModuleName);
        this.mModuleCoverData.put(theModuleName, theModuleData);
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

  /**
   * Parse the line data from the dump.
   *
   * @param inDump    output of the cover dump.
   */
  public void parseLineData(String inDump) {
    final Matcher lineMatcher = COVER_LINE_REGEX.matcher(inDump);
    while (lineMatcher.find()) {
      final String theModuleName = lineMatcher.group(1);

      ModuleCoverData theModuleData = this.mModuleCoverData.get(theModuleName);
      if (theModuleData == null) {
        theModuleData = new ModuleCoverData(theModuleName);
        this.mModuleCoverData.put(theModuleName, theModuleData);
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
    return this.mModuleCoverData.values();
  }

  /**
   * Write the data to an XML file.
   * This function writes the XML header.
   *
   * @param inXMLFile     writer to write to.
   * @throws IOException if there was a problem while writing the file.
   */
  public void writeToXMLFile(Writer inXMLFile) throws IOException {
    inXMLFile.write("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");
    inXMLFile.write("<coverdata>\n");
    if (!this.mModuleCoverData.isEmpty()) {
      inXMLFile.write("  <modules>\n");
      for (ModuleCoverData theModule : this.mModuleCoverData.values()) {
        theModule.writeToXMLFile(inXMLFile, "    ");
      }
      inXMLFile.write("  </modules>\n");
    }
    inXMLFile.write("</coverdata>\n");
  }

  static String escapeXml(String inText) {
    return inText.replaceAll("&", "&amp;")
                 .replaceAll("\"", "&quot;")
                 .replaceAll("<", "&lt;")
                 .replaceAll(">", "&gt;");
  }
}
