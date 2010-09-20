package eu.lindenbaum.maven.cover;

import java.io.IOException;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Class for a function's cover data.
 */
public final class FunctionCoverData {
  /**
   * Name of the function.
   */
  private final String mFunctionName;

  /**
   * Total data for the function.
   */
  private CoverUnit mTotalCoverData;

  /**
   * Data for each clause.
   */
  private final Map<Integer, CoverUnit> mClauseCoverData;

  /**
   * Constructor from a function name and a total cover unit.
   *
   * @param inFunctionName    function identifier (name/arity).
   * @param inTotalCoverData  coverage data for the whole function.
   */
  public FunctionCoverData(String inFunctionName, CoverUnit inTotalCoverData) {
    this.mFunctionName = inFunctionName;
    this.mTotalCoverData = inTotalCoverData;
    this.mClauseCoverData = new HashMap<Integer, CoverUnit>();
  }

  /**
   * Constructor from a function name.
   * Total cover data will be computed later.
   *
   * @param inFunctionName    function identifier (name/arity).
   */
  public FunctionCoverData(String inFunctionName) {
    this.mFunctionName = inFunctionName;
    this.mTotalCoverData = null;
    this.mClauseCoverData = new HashMap<Integer, CoverUnit>();
  }

  /**
   * Append clause data.
   *
   * @param inClauseIndex     index of the clause.
   * @param inData            coverage data for the clause.
   */
  public void putClauseData(Integer inClauseIndex, CoverUnit inData) {
    this.mClauseCoverData.put(inClauseIndex, inData);
  }

  /**
   * Compute total data.
   *
   * @return the total data, i.e. the coverage for the function as a whole.
   */
  private CoverUnit computeTotal() {
    int nbCoveredLines = 0;
    int nbNotCoveredLines = 0;
    for (CoverUnit theUnit : this.mClauseCoverData.values()) {
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
    if (this.mTotalCoverData == null) {
      this.mTotalCoverData = computeTotal();
    }
    return this.mTotalCoverData;
  }

  /**
   * Get the clause cover data.
   *
   * @return a map with clause indexes as the keys and coverage data for the clauses as
   * the values.
   */
  public Map<Integer, CoverUnit> getClauseCoverData() {
    return this.mClauseCoverData;
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
    inXMLFile.write(CoverData.escapeXml(this.mFunctionName));
    inXMLFile.write("\" covered=\"");
    inXMLFile.write(Integer.toString(theTotalCoverData.getCoveredLines()));
    inXMLFile.write("\" notcovered=\"");
    inXMLFile.write(Integer.toString(theTotalCoverData.getNotCoveredLines()));
    inXMLFile.write("\">\n");

    if (!this.mClauseCoverData.isEmpty()) {
      final List<Integer> theClauses = new ArrayList<Integer>(this.mClauseCoverData.keySet());
      Collections.sort(theClauses);
      for (Integer theClause : theClauses) {
        final CoverUnit theClauseCoverData = this.mClauseCoverData.get(theClause);
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
}
