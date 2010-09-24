package eu.lindenbaum.maven.cover;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

public class ModuleCoverDataTest {

  @Test
  public void testEmptyModuleCoverData() {
    ModuleCoverData moduleCoverData = new ModuleCoverData("empty");
    moduleCoverData.calculateStatistics();
    assertEquals("empty", moduleCoverData.getModuleName());
    assertEquals(0, moduleCoverData.getNumberOfFunctions());
    assertEquals(0, moduleCoverData.getNumberOfClauses());
    assertEquals(0, moduleCoverData.getNumberOfLines());
    assertEquals(0, moduleCoverData.getNumberOfCoveredLines());
    assertEquals(0, moduleCoverData.getNumberOfNotCoveredLines());
    assertFalse(moduleCoverData.isCovered());
    assertTrue(moduleCoverData.getFunctionCoverData().isEmpty());
    assertNull(moduleCoverData.getFunctionCoverData("foo"));
    assertTrue(moduleCoverData.getLineCoverData().isEmpty());
    assertNull(moduleCoverData.getLineCoverData(123));
  }
}
