package eu.lindenbaum.maven.cover;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Represents the test coverage information for a single module.
 * 
 * @author Olle Törnström <olle.toernstroem@lindenbaum.eu>
 */
public final class ModuleCoverData {
  private final String moduleName;
  private final Map<String, FunctionCoverData> functionCoverData;
  private final Map<Integer, List<CoverUnit>> lineCoverData;

  private int numberOfClauses;
  private int numberOfLInes;
  private boolean isCovered;
  private int numberOfCoveredLines;
  private int numberOfNotCoveredLines;

  /**
   * Constructor from a name.
   *
   * @param moduleName  name of the module.
   */
  public ModuleCoverData(String moduleName) {
    this.moduleName = moduleName;
    this.functionCoverData = new HashMap<String, FunctionCoverData>();
    this.lineCoverData = new HashMap<Integer, List<CoverUnit>>();
  }

  public void calculateStatistics() {
    for (FunctionCoverData function : this.functionCoverData.values()) {
      this.numberOfClauses += function.getNumberOfClauses();
      this.isCovered &= function.isCovered();
      this.numberOfCoveredLines += function.getCoveredLines();
      this.numberOfNotCoveredLines += function.getNotCoveredLines();
    }
    for (List<CoverUnit> lines : this.lineCoverData.values()) {
      this.numberOfLInes += lines.size();
    }
  }

  /**
   * Fill the module cover data with function data.
   *
   * @param inFunction    identifier of the function (name/arity).
   * @param inUnit        cover data for the function.
   */
  public void putFunctionCoverData(String inFunction, CoverUnit inUnit) {
    this.functionCoverData.put(inFunction, new FunctionCoverData(inFunction, inUnit));
  }

  /**
   * Fill the module cover data with function data.
   *
   * @param inFunction    identifier for the function (name/arity).
   * @param inCoverData   cover data for the function.
   */
  public void putFunctionCoverData(String inFunction, FunctionCoverData inCoverData) {
    this.functionCoverData.put(inFunction, inCoverData);
  }

  /**
   * Get the coverage data for a given function.
   *
   * @param inFunction    identifier for the function (name/arity).
   * @return the cover data for the function.
   */
  public FunctionCoverData getFunctionCoverData(String inFunction) {
    return this.functionCoverData.get(inFunction);
  }

  /**
   * Get the coverage data for all functions.
   *
   * @return the set of the cover data for all functions.
   */
  public Collection<FunctionCoverData> getFunctionCoverData() {
    return this.functionCoverData.values();
  }

  /**
   * Fill the module cover data with line data.
   *
   * @param inLine        line considered.
   * @param inCoverData   cover data for the line.
   */
  public void putLineCoverData(Integer inLine, List<CoverUnit> inCoverData) {
    this.lineCoverData.put(inLine, inCoverData);
  }

  /**
   * Get the coverage data for a given line.
   *
   * @param inLine        line considered.
   * @return the list of cover data units for this line.
   */
  public List<CoverUnit> getLineCoverData(Integer inLine) {
    return this.lineCoverData.get(inLine);
  }

  /**
   * Get the coverage data for all lines.
   *
   * @return a map with line numbers as keys and list of cover data as values.
   */
  public Map<Integer, List<CoverUnit>> getLineCoverData() {
    return this.lineCoverData;
  }

  /**
   * Return the name of the module.
   *
   * @return the name of the module.
   */
  public String getModuleName() {
    return this.moduleName;
  }

  /**
   * Write the module cover data to an XML file. This function writes an XML header.
   *
   * @param inXMLFile writer to write to.
   * @throws IOException if there was a problem while writing the XML.
   */
  public void writeToXMLFile(Writer inXMLFile) throws IOException {
    writeToXMLFile(inXMLFile, (File) null);
  }

  /**
   * Write the module cover data to an XML file. This function writes an XML header.
   *
   * @param inXMLFile     writer to write to.
   * @param inSourceFile  source file for this module (to include source code in the report).
   * @throws IOException if there was a problem while writing the XML.
   */
  public void writeToXMLFile(Writer inXMLFile, File inSourceFile) throws IOException {
    inXMLFile.write("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");
    inXMLFile.write("<coverdata>\n");
    writeToXMLFile(inXMLFile, inSourceFile, "  ");
    inXMLFile.write("</coverdata>\n");
  }

  /**
   * Write the module cover data to an XML file, with a given indent.
   * This function does not write an XML header.
   *
   * @param inXMLFile     writer to write to.
   * @param inIndent      string to prefix all lines.
   * @throws IOException if there was a problem while writing the XML.
   */
  public void writeToXMLFile(Writer inXMLFile, String inIndent) throws IOException {
    writeToXMLFile(inXMLFile, null, inIndent);
  }

  /**
   * Write the module cover data to an XML file, with a given indent.
   * This function does not write an XML header.
   *
   * @param inXMLFile     writer to write to.
   * @param inSourceFile  source file for this module (to include source code in the report).
   * @param inIndent      string to prefix all lines.
   * @throws IOException if there was a problem while writing the XML.
   */
  public void writeToXMLFile(Writer inXMLFile, File inSourceFile, String inIndent) throws IOException {
    inXMLFile.write(inIndent + "<module name=\"");
    inXMLFile.write(CoverData.escapeXml(this.moduleName));
    inXMLFile.write("\">\n");

    if (!this.functionCoverData.isEmpty()) {
      inXMLFile.write(inIndent + "  <functions>\n");
      for (FunctionCoverData theFunction : this.functionCoverData.values()) {
        theFunction.writeToXMLFile(inXMLFile, inIndent + "    ");
      }
      inXMLFile.write(inIndent + "  </functions>\n");
    }
    if (inSourceFile != null || !this.lineCoverData.isEmpty()) {
      final String theLineIndent = inIndent + "    ";
      inXMLFile.write(inIndent + "  <lines>\n");
      if (inSourceFile != null) {
        final BufferedReader theReader = new BufferedReader(new InputStreamReader(new FileInputStream(inSourceFile),
                                                                                  "UTF-8"));
        String theLine = theReader.readLine();
        int theLineIx = 1;
        while (theLine != null) {
          writeLineToXMLFile(inXMLFile, theLine, theLineIx, theLineIndent);
          theLine = theReader.readLine();
          theLineIx++;
        }
      }
      else {
        final List<Integer> theLines = new ArrayList<Integer>(this.lineCoverData.keySet());
        Collections.sort(theLines);
        for (Integer theLineIx : theLines) {
          writeLineToXMLFile(inXMLFile, null, theLineIx, theLineIndent);
        }
      }
      inXMLFile.write(inIndent + "  </lines>\n");
    }

    inXMLFile.write(inIndent + "</module>\n");
  }

  public void writeLineToXMLFile(Writer inXMLFile, String inLine, int inLineIx, String inIndent) throws IOException {
    final List<CoverUnit> theCoverDataList = this.lineCoverData.get(inLineIx);
    int nbCovered = 0;
    int nbNotCovered = 0;
    if (theCoverDataList != null) {
      for (CoverUnit theCoverData : theCoverDataList) {
        nbCovered += theCoverData.getCoveredLines();
        nbNotCovered += theCoverData.getNotCoveredLines();
      }
    }

    if (theCoverDataList != null || inLine != null) {
      inXMLFile.write(inIndent + "    <line line=\"");
      inXMLFile.write(Integer.toString(inLineIx));
      inXMLFile.write("\" covered=\"");
      inXMLFile.write(Integer.toString(nbCovered));
      inXMLFile.write("\" notcovered=\"");
      inXMLFile.write(Integer.toString(nbNotCovered));
      if (inLine != null) {
        inXMLFile.write("\">");
        inXMLFile.write(CoverData.escapeXml(inLine));
        inXMLFile.write("</line>\n");
      }
      else {
        inXMLFile.write("\" />\n");
      }
    }
  }

  public int getNumberOfFunctions() {
    return this.functionCoverData.size();
  }

  public int getNumberOfClauses() {
    return this.numberOfClauses;
  }

  public int getNumberOfLines() {
    return this.numberOfLInes;
  }

  public boolean isCovered() {
    return this.isCovered;
  }

  public int getNumberOfCoveredLines() {
    return this.numberOfCoveredLines;
  }

  public int getNumberOfNotCoveredLines() {
    return this.numberOfNotCoveredLines;
  }
}
