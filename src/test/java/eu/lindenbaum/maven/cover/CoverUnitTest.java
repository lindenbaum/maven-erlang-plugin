package eu.lindenbaum.maven.cover;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

public class CoverUnitTest {

  @Test
  public void testCoverUnitWithOneNotCoveredLine() {
    CoverUnit coverUnit = new CoverUnit(0, 1);
    assertEquals(0, coverUnit.getCoveredLines());
    assertEquals(1, coverUnit.getNotCoveredLines());
    assertFalse(coverUnit.isCovered());
  }

  @Test
  public void testCoverUnitWithOneCoveredLine() {
    CoverUnit coverUnit = new CoverUnit(1, 0);
    assertEquals(1, coverUnit.getCoveredLines());
    assertEquals(0, coverUnit.getNotCoveredLines());
    assertTrue(coverUnit.isCovered());
  }
}
