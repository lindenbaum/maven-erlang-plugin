package eu.lindenbaum.maven.cover;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

public class FunctionCoverDataTest {

  @Test
  public void testEmptyFunctionCoverData() {
    FunctionCoverData functionCoverData = new FunctionCoverData("empty");
    assertEquals("empty", functionCoverData.getFunctionName());
    assertEquals(0, functionCoverData.getNumberOfClauses());
    assertEquals(0, functionCoverData.getCoveredLines());
    assertEquals(0, functionCoverData.getNotCoveredLines());
    assertFalse(functionCoverData.isCovered());
    assertTrue(functionCoverData.getClauseCoverData().isEmpty());
    assertNotNull(functionCoverData.getTotalCoverData());
  }
}
