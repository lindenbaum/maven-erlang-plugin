package eu.lindenbaum.maven.cover;

import java.util.HashMap;
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
   * @param functionName function identifier (name/arity).
   * @param totalCoverData coverage data for the whole function.
   */
  public FunctionCoverData(String functionName, CoverUnit totalCoverData) {
    this.functionName = functionName;
    this.totalCoverData = totalCoverData;
    this.clauseCoverData = new HashMap<Integer, CoverUnit>();
  }

  /**
   * Constructor from a function name. Total cover data will be computed later.
   * 
   * @param functionName function identifier (name/arity).
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
   * @param inClauseIndex index of the clause.
   * @param inData coverage data for the clause.
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
   * @return a map with clause indexes as the keys and coverage data for the
   *         clauses as the values.
   */
  public Map<Integer, CoverUnit> getClauseCoverData() {
    return this.clauseCoverData;
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
