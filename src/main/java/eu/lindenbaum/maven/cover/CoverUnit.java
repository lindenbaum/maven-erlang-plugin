package eu.lindenbaum.maven.cover;

/**
 * Class for a cover unit, i.e. a number of lines that were covered and a number
 * of lines that were not covered.
 */
public final class CoverUnit {
  private final int coveredLines;
  private final int notCoveredLines;
  private final boolean isCovered;

  /**
   * Constructor from a number of lines that were covered and a number of lines
   * that were not covered.
   * 
   * @param inCoveredLines number of lines covered.
   * @param inNotCoveredLines number of lines not covered.
   */
  public CoverUnit(int inCoveredLines, int inNotCoveredLines) {
    this.coveredLines = inCoveredLines;
    this.notCoveredLines = inNotCoveredLines;
    this.isCovered = (this.notCoveredLines == 0 && this.coveredLines > 0);
  }

  /**
   * Accessor on the number of lines that were covered.
   * 
   * @return the number of lines that were covered.
   */
  public int getCoveredLines() {
    return this.coveredLines;
  }

  /**
   * Accessor on the number of lines that were not covered.
   * 
   * @return the number of lines that were not covered.
   */
  public int getNotCoveredLines() {
    return this.notCoveredLines;
  }

  public boolean isCovered() {
    return this.isCovered;
  }
}
