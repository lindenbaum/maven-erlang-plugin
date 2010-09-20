package eu.lindenbaum.maven.cover;

/**
 * Class for a cover unit, i.e. a number of lines that were covered and a
 * number of lines that were not covered.
 */
public final class CoverUnit {
  /**
   * Number of lines that were covered.
   */
  private final int mCoveredLines;

  /**
   * Number of lines that were not covered.
   */
  private final int mNotCoveredLines;

  /**
   * Constructor from a number of lines that were covered and a number of
   * lines that were not covered.
   *
   * @param inCoveredLines    number of lines covered.
   * @param inNotCoveredLines number of lines not covered.
   */
  public CoverUnit(int inCoveredLines, int inNotCoveredLines) {
    this.mCoveredLines = inCoveredLines;
    this.mNotCoveredLines = inNotCoveredLines;
  }

  /**
   * Accessor on the number of lines that were covered.
   *
   * @return the number of lines that were covered.
   */
  public int getCoveredLines() {
    return this.mCoveredLines;
  }

  /**
   * Accessor on the number of lines that were not covered.
   *
   * @return the number of lines that were not covered.
   */
  public int getNotCoveredLines() {
    return this.mNotCoveredLines;
  }
}
