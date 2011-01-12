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

  @Test
  public void testPlainTextFormatting() {
    assertEquals("Total coverage:                                                      99%",
                 String.format("Total coverage:%1$56d%%", 99));
    assertEquals("Modules:                        19 | Lines:                          394",
                 String.format("Modules: %1$25d | Lines: %2$28d", 19, 394));
    assertEquals("Functions:                     159 | Covered lines:                  393",
                 String.format("Functions: %1$23d | Covered lines: %2$20d", 159, 393));
    assertEquals("Clauses:                       181 | Not covered lines:                1",
                 String.format("Clauses: %1$25d | Not covered lines: %2$16d", 181, 1));
    String name1 = "config_evt.erl";
    assertEquals("> config_evt.erl                                                COVERED!",
                 String.format("> %1$s %2$" + (69 - name1.length()) + "s", name1, "COVERED!"));
    String name2 = "dce_handler.erl";
    assertEquals("> dce_handler.erl                                           NOT COVERED!",
                 String.format("> %1$s %2$" + (69 - name2.length()) + "s", name2, "NOT COVERED!"));
    assertEquals("Coverage:                      99% | Lines:                          394",
                 String.format("Coverage: %1$23d%% | Lines: %2$28d", 99, 394));
    assertEquals("Functions:                     159 | Covered lines:                  393",
                 String.format("Functions: %1$23d | Covered lines: %2$20d", 159, 393));
    assertEquals("Clauses:                       181 | Not covered lines:                1",
                 String.format("Clauses: %1$25d | Not covered lines: %2$16d", 181, 1));
  }
}
