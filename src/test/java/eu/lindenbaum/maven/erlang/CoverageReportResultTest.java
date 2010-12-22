package eu.lindenbaum.maven.erlang;

import static org.junit.Assert.assertEquals;
import eu.lindenbaum.maven.erlang.CoverageReportResult.Report;

import org.junit.Test;

public class CoverageReportResultTest {

  @Test
  public void testCalculateCoverage() {
    assertEquals(25, Report.calculateCoverage(1, 3));
    assertEquals(33, Report.calculateCoverage(1, 2));
    assertEquals(50, Report.calculateCoverage(2, 2));
    assertEquals(66, Report.calculateCoverage(2, 1));
    assertEquals(75, Report.calculateCoverage(3, 1));
    assertEquals(100, Report.calculateCoverage(3, 0));
  }
}
