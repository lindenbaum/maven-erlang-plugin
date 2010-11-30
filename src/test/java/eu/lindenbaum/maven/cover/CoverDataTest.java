package eu.lindenbaum.maven.cover;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import org.junit.Before;
import org.junit.Test;

public class CoverDataTest {
  private static final String TEST_COVERAGE_DUMP = "[[{ok,[{{test_app,start,2},{0,3}},{{test_app,stop,1},{0,1}},{{test_app,internal_fun,0},{1,0}}]},{ok,[{{test_app,start,2,1},{0,3}},{{test_app,stop,1,1},{0,1}},{{test_app,internal_fun,0,1},{1,0}}]},{ok,[{{test_app,37},{0,1}},{{test_app,39},{0,1}},{{test_app,41},{0,1}},{{test_app,54},{0,1}},{{test_app,61},{1,0}}]}],[{ok,[{{test_server,start_link,0},{0,1}},{{test_server,test,1},{0,1}},{{test_server,current_time_millis,0},{2,0}},{{test_server,init,1},{0,1}},{{test_server,handle_call,3},{0,2}},{{test_server,handle_cast,2},{0,4}},{{test_server,handle_info,2},{0,1}},{{test_server,terminate,2},{0,1}},{{test_server,code_change,3},{0,1}},{{test_server,test_round,1},{0,5}},{{test_server,test_round,2},{1,2}}]},{ok,[{{test_server,start_link,0,1},{0,1}},{{test_server,test,1,1},{0,1}},{{test_server,current_time_millis,0,1},{2,0}},{{test_server,init,1,1},{0,1}},{{test_server,handle_call,3,1},{0,2}},{{test_server,handle_cast,2,1},{0,2}},{{test_server,handle_cast,2,2},{0,2}},{{test_server,handle_info,2,1},{0,1}},{{test_server,terminate,2,1},{0,1}},{{test_server,code_change,3,1},{0,1}},{{test_server,test_round,1,1},{0,5}},{{test_server,test_round,2,1},{1,0}},{{test_server,test_round,2,2},{0,2}}]},{ok,[{{test_server,34},{0,1}},{{test_server,44},{0,1}},{{test_server,55},{1,0}},{{test_server,56},{1,0}},{{test_server,71},{0,1}},{{test_server,83},{0,1}},{{test_server,84},{0,1}},{{test_server,95},{0,1}},{{test_server,96},{0,1}},{{test_server,98},{0,1}},{{test_server,99},{0,1}},{{test_server,110},{0,1}},{{test_server,124},{0,1}},{{test_server,135},{0,1}},{{test_server,142},{0,1}},{{test_server,143},{0,1}},{{test_server,144},{0,1}},{{test_server,145},{0,1}},{{test_server,146},{0,1}},{{test_server,149},{1,0}},{{test_server,151},{0,1}},{{test_server,152},{0,1}}]}]]";
  private static final String TEST_COVERAGE_DUMP2 = "[{{glotzer,start,2},{0,3}},{{glotzer,stop,1},{1,0}},{{glotzer,start_service,0},{0,2}},{{glotzer,start_webserver,0},{0,3}},{{glotzer,loop,1},{0,14}},{{glotzer,get_participant_log_entry,0},{1,0}},{{glotzer,get_conference_log_entry,0},{1,0}},{{glotzer,epoch,0},{2,0}},{{glotzer,participant,0},{1,0}},{{glotzer,action,1},{1,0}},{{glotzer,conference,0},{1,0}}]},{ok,[{{glotzer,start,2,1},{0,3}},{{glotzer,stop,1,1},{1,0}},{{glotzer,start_service,0,1},{0,2}},{{glotzer,start_webserver,0,1},{0,3}},{{glotzer,loop,1,1},{0,14}},{{glotzer,get_participant_log_entry,0,1},{1,0}},{{glotzer,get_conference_log_entry,0,1},{1,0}},{{glotzer,epoch,0,1},{2,0}},{{glotzer,participant,0,1},{1,0}},{{glotzer,action,1,1},{1,0}},{{glotzer,conference,0,1},{1,0}}]},{ok,[{{glotzer,15},{0,1}},{{glotzer,16},{0,1}},{{glotzer,17},{0,1}},{{glotzer,20},{1,0}},{{glotzer,23},{0,1}},{{glotzer,24},{0,1}},{{glotzer,27},{0,1}},{{glotzer,28},{0,1}},{{glotzer,29},{0,1}},{{glotzer,32},{0,1}},{{glotzer,34},{0,1}},{{glotzer,36},{0,1}},{{glotzer,37},{0,1}},{{glotzer,38},{0,1}},{{glotzer,40},{0,1}},{{glotzer,42},{0,1}},{{glotzer,43},{0,1}},{{glotzer,44},{0,1}},{{glotzer,46},{0,1}},{{glotzer,47},{0,1}},{{glotzer,48},{0,1}},{{glotzer,50},{0,1}},{{glotzer,51},{0,1}},{{glotzer,55},{1,0}},{{glotzer,58},{1,0}},{{glotzer,61},{1,0}},{{glotzer,62},{1,0}},{{glotzer,65},{1,0}},{{glotzer,68},{1,0}},{{glotzer,71},{1,0}}]}],[{ok,[{{glotzer_protocol,decode,1},{3,0}},{{glotzer_protocol,decode,2},{6,0}},{{glotzer_protocol,encode,1},{2,0}},{{glotzer_protocol,encode_message,1},{1,0}},{{glotzer_protocol,encode_message,2},{2,0}},{{glotzer_protocol,encode_code,1},{3,0}},{{glotzer_protocol,decode_code,1},{1,0}},{{glotzer_protocol,dec2hex,1},{1,0}},{{glotzer_protocol,dec2hex,2},{2,0}},{{glotzer_protocol,hex2dec,1},{3,0}}]},{ok,[{{glotzer_protocol,decode,1,1},{3,0}},{{glotzer_protocol,decode,2,1},{1,0}},{{glotzer_protocol,decode,2,2},{5,0}},{{glotzer_protocol,encode,1,1},{1,0}},{{glotzer_protocol,encode,1,2},{1,0}},{{glotzer_protocol,encode_message,1,1},{1,0}},{{glotzer_protocol,encode_message,2,1},{1,0}},{{glotzer_protocol,encode_message,2,2},{1,0}},{{glotzer_protocol,encode_code,1,1},{3,0}},{{glotzer_protocol,decode_code,1,1},{1,0}},{{glotzer_protocol,dec2hex,1,1},{1,0}},{{glotzer_protocol,dec2hex,2,1},{2,0}},{{glotzer_protocol,hex2dec,1,1},{3,0}}]},{ok,[{{glotzer_protocol,21},{1,0}},{{glotzer_protocol,22},{1,0}},{{glotzer_protocol,23},{1,0}},{{glotzer_protocol,27},{1,0}},{{glotzer_protocol,30},{1,0}},{{glotzer_protocol,31},{1,0}},{{glotzer_protocol,32},{1,0}},{{glotzer_protocol,33},{1,0}},{{glotzer_protocol,34},{1,0}},{{glotzer_protocol,42},{1,0}},{{glotzer_protocol,44},{1,0}},{{glotzer_protocol,49},{1,0}},{{glotzer_protocol,50},{1,0}},{{glotzer_protocol,51},{1,0}},{{glotzer_protocol,57},{1,0}},{{glotzer_protocol,58},{1,0}},{{glotzer_protocol,59},{1,0}},{{glotzer_protocol,65},{1,0}},{{glotzer_protocol,71},{1,0}},{{glotzer_protocol,77},{1,0}},{{glotzer_protocol,78},{1,0}},{{glotzer_protocol,84},{1,0}},{{glotzer_protocol,85},{1,0}},{{glotzer_protocol,86},{1,0}}]";
  private static final String TEST_FULL_COVERAGE = "[[{ok,[{{mod_a,fun_a,2},{3,0}},{{mod_a,fun_b,1},{1,0}},{{mod_a,fun_c,0},{1,0}}]},{ok,[{{mod_a,fun_a,2,1},{3,0}},{{mod_a,fun_b,1,1},{1,0}},{{mod_a,fun_c,0,1},{1,0}}]},{ok,[{{mod_a,37},{1,0}},{{mod_a,39},{1,0}},{{mod_a,41},{1,0}},{{mod_a,54},{1,0}},{{mod_a,61},{1,0}}]}],[{ok,[{{mod_b,fun_a,2},{3,0}},{{mod_b,fun_b,1},{1,0}},{{mod_b,fun_c,0},{1,0}}]},{ok,[{{mod_b,fun_a,2,1},{3,0}},{{mod_b,fun_b,1,1},{1,0}},{{mod_b,fun_c,0,1},{1,0}}]},{ok,[{{mod_b,37},{1,0}},{{mod_b,39},{1,0}},{{mod_b,41},{1,0}},{{mod_b,54},{1,0}},{{mod_b,61},{1,0}}]}]]";

  private CoverData coverData;
  private CoverData coverData2;
  private CoverData fullCoverage;

  @Before
  public void setUp() {
    this.coverData = new CoverData(TEST_COVERAGE_DUMP);
    this.coverData2 = new CoverData(TEST_COVERAGE_DUMP2);
    this.fullCoverage = new CoverData(TEST_FULL_COVERAGE);
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

  @Test
  public void testCoverDataWithBetterCoverage() {
    for (ModuleCoverData module : this.coverData2.getModuleCoverData()) {
      int notCoveredLines = module.getNumberOfNotCoveredLines();
      int coveredLines = module.getNumberOfCoveredLines();
      assertTrue(notCoveredLines != coveredLines);
      if (notCoveredLines == 0) {
        assertTrue("Module: " + module.getModuleName() + " should be covered, has " + notCoveredLines
                   + " non covered lines and " + coveredLines + " covered lines.", module.isCovered());
      }
      else {
        assertFalse(module.isCovered());
      }
    }
  }

  @Test
  public void testFullCoverage() {
    assertTrue(this.fullCoverage.isCovered());
    assertEquals(this.fullCoverage.getNumberOfLines(), this.fullCoverage.getNumberOfCoveredLines());
    assertEquals(0, this.fullCoverage.getNumberOfNotCoveredLines());
    for (ModuleCoverData module : this.fullCoverage.getModuleCoverData()) {
      assertTrue(module.isCovered());
      assertEquals(module.getNumberOfLines(), module.getNumberOfCoveredLines());
      assertEquals(0, module.getNumberOfNotCoveredLines());
    }
  }
}
