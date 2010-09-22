package eu.lindenbaum.maven.cover;

import java.io.IOException;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Represents the test coverage information for some function.
 * 
 * @author Olle Törnström <olle.toernstroem@lindenbaum.eu>
 */
public final class FunctionCoverData {
  private final String functionName;
  private CoverUnit totalCoverData;
  private final Map<Integer, CoverUnit> clauseCoverData;

  /**
   * Constructor from a function name and a total cover unit.
   *
   * @param functionName    function identifier (name/arity).
   * @param totalCoverData  coverage data for the whole function.
   */
  public FunctionCoverData(String functionName, CoverUnit totalCoverData) {
    this.functionName = functionName;
    this.totalCoverData = totalCoverData;
    this.clauseCoverData = new HashMap<Integer, CoverUnit>();
  }

  /**
   * Constructor from a function name.
   * Total cover data will be computed later.
   *
   * @param functionName    function identifier (name/arity).
   */
  public FunctionCoverData(String functionName) {
    this(functionName, null);
  }

  public String getFunctionName() {
    return this.functionName;
  }

  /**
   * Append clause data.
   *
   * @param inClauseIndex     index of the clause.
   * @param inData            coverage data for the clause.
   */
  public void putClauseData(Integer inClauseIndex, CoverUnit inData) {
    this.clauseCoverData.put(inClauseIndex, inData);
  }

  /**
   * Compute total data.
   *
   * @return the total data, i.e. the coverage for the function as a whole.
   */
  private CoverUnit computeTotal() {
    int nbCoveredLines = 0;
    int nbNotCoveredLines = 0;
    for (CoverUnit theUnit : this.clauseCoverData.values()) {
      nbCoveredLines += theUnit.getCoveredLines();
      nbNotCoveredLines += theUnit.getNotCoveredLines();
    }
    return new CoverUnit(nbCoveredLines, nbNotCoveredLines);
  }

  /**
   * Get the total cover data, computing it if required.
   *
   * @return the total data, i.e. the coverage for the function as a whole.
   */
  public CoverUnit getTotalCoverData() {
    if (this.totalCoverData == null) {
      this.totalCoverData = computeTotal();
    }
    return this.totalCoverData;
  }

  /**
   * Get the clause cover data.
   *
   * @return a map with clause indexes as the keys and coverage data for the clauses as
   * the values.
   */
  public Map<Integer, CoverUnit> getClauseCoverData() {
    return this.clauseCoverData;
  }

  /**
   * Write the function cover data to an XML file, with a given indent.
   *
   * @param inXMLFile     writer to write to.
   * @param inIndent      prefix for all lines.
   * @throws IOException  if a problem occurred while writing the XML file.
   */
  public void writeToXMLFile(Writer inXMLFile, String inIndent) throws IOException {
    final CoverUnit theTotalCoverData = getTotalCoverData();
    inXMLFile.write(inIndent + "<function name=\"");
    inXMLFile.write(CoverData.escapeXml(this.functionName));
    inXMLFile.write("\" covered=\"");
    inXMLFile.write(Integer.toString(theTotalCoverData.getCoveredLines()));
    inXMLFile.write("\" notcovered=\"");
    inXMLFile.write(Integer.toString(theTotalCoverData.getNotCoveredLines()));
    inXMLFile.write("\">\n");

    if (!this.clauseCoverData.isEmpty()) {
      final List<Integer> theClauses = new ArrayList<Integer>(this.clauseCoverData.keySet());
      Collections.sort(theClauses);
      for (Integer theClause : theClauses) {
        final CoverUnit theClauseCoverData = this.clauseCoverData.get(theClause);
        inXMLFile.write(inIndent + "  <clause index=\"");
        inXMLFile.write(Integer.toString(theClause));
        inXMLFile.write("\" covered=\"");
        inXMLFile.write(Integer.toString(theClauseCoverData.getCoveredLines()));
        inXMLFile.write("\" notcovered=\"");
        inXMLFile.write(Integer.toString(theClauseCoverData.getNotCoveredLines()));
        inXMLFile.write("\" />\n");
      }
    }
    inXMLFile.write(inIndent + "</function>\n");
  }

  public int getNumberOfClauses() {
    return this.clauseCoverData.size();
  }

  public int getCoveredLines() {
    return this.getTotalCoverData().getCoveredLines();
  }

  public int getNotCoveredLines() {
    return this.getTotalCoverData().getNotCoveredLines();
  }

  public boolean isCovered() {
    return this.getTotalCoverData().isCovered();
  }
}
