package eu.lindenbaum.maven;

import static org.easymock.EasyMock.anyObject;
import static org.easymock.EasyMock.createStrictControl;
import static org.easymock.EasyMock.expectLastCall;
import static org.easymock.EasyMock.isA;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;
import java.util.Locale;

import eu.lindenbaum.maven.cover.CoverData;
import eu.lindenbaum.maven.cover.ModuleCoverData;

import org.apache.maven.doxia.sink.Sink;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;
import org.apache.maven.reporting.MavenReportException;
import org.easymock.IMocksControl;
import org.junit.Before;
import org.junit.Test;

public class CoverageReportTest {
  private static final Locale LOCALE = Locale.getDefault();
  private IMocksControl control;

  Sink sink;
  Log log;

  @Before
  public void setUp() {
    this.control = createStrictControl();
    this.sink = this.control.createMock("sink", Sink.class);
    this.log = this.control.createMock("log", Log.class);
  }

  abstract class LocalCoverageReport extends CoverageReport {
    {
      this.coverData = new CoverData("[[{ok,[{{test_app,start,2},{0,3}},{{test_app,stop,1},{0,1}},{{test_app,internal_fun,0},{1,0}}]},{ok,[{{test_app,start,2,1},{0,3}},{{test_app,stop,1,1},{0,1}},{{test_app,internal_fun,0,1},{1,0}}]},{ok,[{{test_app,37},{0,1}},{{test_app,39},{0,1}},{{test_app,41},{0,1}},{{test_app,54},{0,1}},{{test_app,61},{1,0}}]}],[{ok,[{{test_server,start_link,0},{0,1}},{{test_server,test,1},{0,1}},{{test_server,current_time_millis,0},{2,0}},{{test_server,init,1},{0,1}},{{test_server,handle_call,3},{0,2}},{{test_server,handle_cast,2},{0,4}},{{test_server,handle_info,2},{0,1}},{{test_server,terminate,2},{0,1}},{{test_server,code_change,3},{0,1}},{{test_server,test_round,1},{0,5}},{{test_server,test_round,2},{1,2}}]},{ok,[{{test_server,start_link,0,1},{0,1}},{{test_server,test,1,1},{0,1}},{{test_server,current_time_millis,0,1},{2,0}},{{test_server,init,1,1},{0,1}},{{test_server,handle_call,3,1},{0,2}},{{test_server,handle_cast,2,1},{0,2}},{{test_server,handle_cast,2,2},{0,2}},{{test_server,handle_info,2,1},{0,1}},{{test_server,terminate,2,1},{0,1}},{{test_server,code_change,3,1},{0,1}},{{test_server,test_round,1,1},{0,5}},{{test_server,test_round,2,1},{1,0}},{{test_server,test_round,2,2},{0,2}}]},{ok,[{{test_server,34},{0,1}},{{test_server,44},{0,1}},{{test_server,55},{1,0}},{{test_server,56},{1,0}},{{test_server,71},{0,1}},{{test_server,83},{0,1}},{{test_server,84},{0,1}},{{test_server,95},{0,1}},{{test_server,96},{0,1}},{{test_server,98},{0,1}},{{test_server,99},{0,1}},{{test_server,110},{0,1}},{{test_server,124},{0,1}},{{test_server,135},{0,1}},{{test_server,142},{0,1}},{{test_server,143},{0,1}},{{test_server,144},{0,1}},{{test_server,145},{0,1}},{{test_server,146},{0,1}},{{test_server,149},{1,0}},{{test_server,151},{0,1}},{{test_server,152},{0,1}}]}]]");
    }
  }

  @Test
  public void testCoverageReport() throws MavenReportException {
    mockExpectConstructHeader();
    this.sink.body();
    this.sink.section1();
    this.sink.sectionTitle1();
    this.sink.text((String) anyObject());
    this.sink.sectionTitle1_();
    this.sink.paragraph();
    this.sink.text((String) anyObject());
    this.sink.paragraph_();
    this.sink.section1_();
    this.control.checkOrder(false);
    stubMockExpects();
    this.control.checkOrder(true);
    this.sink.body_();
    this.sink.flush();
    this.sink.close();
    this.control.replay();
    LocalCoverageReport coverageReport = new LocalCoverageReport() {
      @Override
      public Sink getSink() {
        return CoverageReportTest.this.sink;
      }

      @Override
      void constructModuleLineCoverage(Sink sink, ModuleCoverData module) {
        // Tested only in integration test, see src/it/coverage.
      }
    };
    coverageReport.executeReport(LOCALE);
    this.control.verify();
  }

  private void mockExpectConstructHeader() {
    this.sink.head();
    this.sink.title();
    this.sink.text((String) anyObject());
    this.sink.title_();
    this.sink.head_();
  }

  private void stubMockExpects() {
    stubMockH2();
    stubMockH3();
    stubMockText();
    stubMockTable();
    stubMockImg();
    stubMockLink();
  }

  private void stubMockLink() {
    this.sink.link((String) anyObject());
    expectLastCall().asStub();
    this.sink.link_();
    expectLastCall().asStub();
    this.sink.anchor((String) anyObject());
    expectLastCall().asStub();
    this.sink.anchor_();
    expectLastCall().asStub();
  }

  private void stubMockImg() {
    this.sink.figure();
    expectLastCall().asStub();
    this.sink.figure_();
    expectLastCall().asStub();
    this.sink.figureGraphics((String) anyObject());
    expectLastCall().asStub();
  }

  private void stubMockTable() {
    this.sink.table();
    expectLastCall().asStub();
    this.sink.table_();
    expectLastCall().asStub();
    this.sink.tableRow();
    expectLastCall().asStub();
    this.sink.tableRow_();
    expectLastCall().asStub();
    this.sink.tableHeaderCell();
    expectLastCall().asStub();
    this.sink.tableHeaderCell_();
    expectLastCall().asStub();
    this.sink.tableCell();
    expectLastCall().asStub();
    this.sink.tableCell_();
    expectLastCall().asStub();
  }

  private void stubMockText() {
    this.sink.text((String) anyObject());
    expectLastCall().asStub();
  }

  private void stubMockH3() {
    this.sink.section3();
    expectLastCall().asStub();
    this.sink.section3_();
    expectLastCall().asStub();
    this.sink.sectionTitle3();
    expectLastCall().asStub();
    this.sink.sectionTitle3_();
    expectLastCall().asStub();
  }

  private void stubMockH2() {
    this.sink.section2();
    expectLastCall().asStub();
    this.sink.section2_();
    expectLastCall().asStub();
    this.sink.sectionTitle2();
    expectLastCall().asStub();
    this.sink.sectionTitle2_();
    expectLastCall().asStub();
  }

  @Test
  public void testUnableToGenerateReport() throws MavenReportException {
    this.log.info((CharSequence) anyObject());
    this.control.replay();
    LocalCoverageReport coverageReport = new LocalCoverageReport() {
      @Override
      public boolean canGenerateReport() {
        return false;
      }

      @Override
      public Log getLog() {
        return CoverageReportTest.this.log;
      }
    };
    coverageReport.executeReport(LOCALE);
    this.control.verify();
  }

  @Test
  public void testLoadCoverageReportThrowException() {
    this.control.replay();
    try {
      new LocalCoverageReport() {
        @Override
        void loadCoverageData() throws MojoExecutionException {
          throw new MojoExecutionException("expected");
        }

        @Override
        public Log getLog() {
          return CoverageReportTest.this.log;
        }
      }.executeReport(LOCALE);
      fail("MavenReportException expected");
    }
    catch (MavenReportException expected) {
      // OK
    }
    catch (Throwable e) {
      assertEquals(MavenReportException.class, e.getClass());
    }
    this.control.verify();
  }

  @Test
  public void testGetOutputDirectory() {
    String actual = new LocalCoverageReport() {
      {
        this.targetSiteCoverage = new File("actual");
      }
    }.getOutputDirectory();
    assertTrue(actual.startsWith("/") && actual.endsWith("/actual"));
  }

  @Test
  public void testGetName() {
    String actual = new CoverageReport().getName(LOCALE);
    assertEquals("Coverage", actual);
  }

  @Test
  public void testGetOutputName() {
    String actual = new CoverageReport().getOutputName();
    assertEquals("erlang-coverage-report", actual);
  }

  @Test
  public void testIsExternal() {
    boolean actual = new CoverageReport().isExternalReport();
    assertEquals(false, actual);
  }

  @Test
  public void canGenerateReportThrowsException() {
    this.log.error((CharSequence) anyObject(), isA(MojoExecutionException.class));
    this.control.replay();
    boolean actual = new LocalCoverageReport() {
      @Override
      void loadCoverageData() throws MojoExecutionException {
        throw new MojoExecutionException("expected");
      }

      @Override
      public Log getLog() {
        return CoverageReportTest.this.log;
      }
    }.canGenerateReport();
    assertEquals(false, actual);
    this.control.verify();
  }
}
