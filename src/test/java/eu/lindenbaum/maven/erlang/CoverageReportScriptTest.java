package eu.lindenbaum.maven.erlang;

import static org.easymock.EasyMock.createStrictControl;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.util.Arrays;
import java.util.List;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;
import org.easymock.IMocksControl;
import org.junit.Before;
import org.junit.Test;

public class CoverageReportScriptTest {
  private IMocksControl control;
  private Log log;

  @Before
  public void setUp() {
    this.control = createStrictControl();
    this.log = this.control.createMock("log", Log.class);
  }

  @Test
  public void testGet() throws MojoExecutionException {
    File testDir = new File("testDir");
    List<File> tests = Arrays.asList(new File("test1"), new File("test2"));
    List<File> sources = Arrays.asList(new File("source1"), new File("source2"));
    File targetDir = new File("targetDir");
    CoverageReportScript script = new CoverageReportScript(testDir, tests, sources, targetDir, "targetName");
    String expression = script.get();
    assertNotNull(expression);
    assertFalse(expression.isEmpty());
    assertFalse(expression.contains("%s"));
  }

  @Test
  public void testCoverageFailed() throws MojoExecutionException {
    this.log.error("");
    this.log.error("message2");
    this.log.error("message3");

    this.control.replay();

    OtpErlangList message1 = new OtpErlangList();
    OtpErlangString message2 = new OtpErlangString("message2");
    OtpErlangString message3 = new OtpErlangString("message3");
    OtpErlangObject[] m = new OtpErlangObject[]{ message1, message2, message3 };
    OtpErlangList messages = new OtpErlangList(m);

    OtpErlangAtom level = new OtpErlangAtom("error");

    OtpErlangTuple result = new OtpErlangTuple(new OtpErlangObject[]{ level, messages });

    File testDir = new File("testDir");
    List<File> tests = Arrays.asList(new File("test1"), new File("test2"));
    List<File> sources = Arrays.asList(new File("source1"), new File("source2"));
    File targetDir = new File("targetDir");

    CoverageReportScript script = new CoverageReportScript(testDir, tests, sources, targetDir, "targetName");
    CoverageReportResult coverageResult = script.handle(result);

    assertTrue(coverageResult.failed());
    coverageResult.logOutput(this.log);

    this.control.verify();
  }
}
