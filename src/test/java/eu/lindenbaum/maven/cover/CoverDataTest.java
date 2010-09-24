package eu.lindenbaum.maven.cover;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;

import org.junit.Before;
import org.junit.Test;

public class CoverDataTest {
  private static final String TEST_COVERAGE_DUMP = "[[{ok,[{{test_app,start,2},{0,3}},{{test_app,stop,1},{0,1}},{{test_app,internal_fun,0},{1,0}}]},{ok,[{{test_app,start,2,1},{0,3}},{{test_app,stop,1,1},{0,1}},{{test_app,internal_fun,0,1},{1,0}}]},{ok,[{{test_app,37},{0,1}},{{test_app,39},{0,1}},{{test_app,41},{0,1}},{{test_app,54},{0,1}},{{test_app,61},{1,0}}]}],[{ok,[{{test_server,start_link,0},{0,1}},{{test_server,test,1},{0,1}},{{test_server,current_time_millis,0},{2,0}},{{test_server,init,1},{0,1}},{{test_server,handle_call,3},{0,2}},{{test_server,handle_cast,2},{0,4}},{{test_server,handle_info,2},{0,1}},{{test_server,terminate,2},{0,1}},{{test_server,code_change,3},{0,1}},{{test_server,test_round,1},{0,5}},{{test_server,test_round,2},{1,2}}]},{ok,[{{test_server,start_link,0,1},{0,1}},{{test_server,test,1,1},{0,1}},{{test_server,current_time_millis,0,1},{2,0}},{{test_server,init,1,1},{0,1}},{{test_server,handle_call,3,1},{0,2}},{{test_server,handle_cast,2,1},{0,2}},{{test_server,handle_cast,2,2},{0,2}},{{test_server,handle_info,2,1},{0,1}},{{test_server,terminate,2,1},{0,1}},{{test_server,code_change,3,1},{0,1}},{{test_server,test_round,1,1},{0,5}},{{test_server,test_round,2,1},{1,0}},{{test_server,test_round,2,2},{0,2}}]},{ok,[{{test_server,34},{0,1}},{{test_server,44},{0,1}},{{test_server,55},{1,0}},{{test_server,56},{1,0}},{{test_server,71},{0,1}},{{test_server,83},{0,1}},{{test_server,84},{0,1}},{{test_server,95},{0,1}},{{test_server,96},{0,1}},{{test_server,98},{0,1}},{{test_server,99},{0,1}},{{test_server,110},{0,1}},{{test_server,124},{0,1}},{{test_server,135},{0,1}},{{test_server,142},{0,1}},{{test_server,143},{0,1}},{{test_server,144},{0,1}},{{test_server,145},{0,1}},{{test_server,146},{0,1}},{{test_server,149},{1,0}},{{test_server,151},{0,1}},{{test_server,152},{0,1}}]}]]";
  private CoverData coverData;

  @Before
  public void setUp() {
    this.coverData = new CoverData(TEST_COVERAGE_DUMP);
  }

  @Test
  public void testCoverDataStatisticsInformation() {
    assertEquals(2, this.coverData.getNumberOfModules());
    assertEquals(14, this.coverData.getNumberOfFunctions());
    assertEquals(16, this.coverData.getNumberOfClauses());
    assertEquals(27, this.coverData.getNumberOfLines());
    assertEquals(4, this.coverData.getNumberOfCoveredLines());
    assertEquals(23, this.coverData.getNumberOfNotCoveredLines());
    assertNotNull(this.coverData.getModuleCoverData());
    assertEquals(2, this.coverData.getModuleCoverData().size());
    assertFalse(this.coverData.isCovered());
  }
}
