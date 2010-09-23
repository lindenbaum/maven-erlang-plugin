package eu.lindenbaum.maven.cover;

import java.util.Collection;
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
