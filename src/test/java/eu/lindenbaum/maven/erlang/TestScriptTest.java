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

public class TestScriptTest {
  private IMocksControl control;
  private Log log;

  @Before
  public void setUp() {
    this.control = createStrictControl();
    this.log = this.control.createMock("log", Log.class);
  }

  @Test
  public void testGet() throws MojoExecutionException {
    List<File> tests = Arrays.asList(new File("test1"), new File("test2"));
    File surefireDir = new File("surefireDir");
    String suiteName = "suiteName";

    TestScript script = new TestScript(tests, surefireDir, suiteName);
    String expression = script.get();
    assertNotNull(expression);
    assertFalse(expression.isEmpty());
    assertFalse(expression.contains("%s"));
  }

  @Test
  public void testHandleTestsPassed() throws MojoExecutionException {
    this.log.info("");
    this.log.info("message2");
    this.log.info("message3");

    this.control.replay();

    OtpErlangList message1 = new OtpErlangList();
    OtpErlangString message2 = new OtpErlangString("message2");
    OtpErlangString message3 = new OtpErlangString("message3");
    OtpErlangObject[] m = new OtpErlangObject[]{ message1, message2, message3 };
    OtpErlangList messages = new OtpErlangList(m);

    OtpErlangAtom level = new OtpErlangAtom("info");

    OtpErlangTuple result = new OtpErlangTuple(new OtpErlangObject[]{ level, messages });

    List<File> tests = Arrays.asList(new File("test1"), new File("test2"));
    File surefireDir = new File("surefireDir");
    String suiteName = "suiteName";

    TestScript script = new TestScript(tests, surefireDir, suiteName);
    TestResult testResult = script.handle(result);
    assertTrue(testResult.testsPassed());
    testResult.logOutput(this.log);

    this.control.verify();
  }

  @Test
  public void testHandleTestsPassedWithWarnings() throws MojoExecutionException {
    this.log.warn("");
    this.log.warn("message2");
    this.log.warn("message3");

    this.control.replay();

    OtpErlangList message1 = new OtpErlangList();
    OtpErlangString message2 = new OtpErlangString("message2");
    OtpErlangString message3 = new OtpErlangString("message3");
    OtpErlangObject[] m = new OtpErlangObject[]{ message1, message2, message3 };
    OtpErlangList messages = new OtpErlangList(m);

    OtpErlangAtom level = new OtpErlangAtom("warn");

    OtpErlangTuple result = new OtpErlangTuple(new OtpErlangObject[]{ level, messages });

    List<File> tests = Arrays.asList(new File("test1"), new File("test2"));
    File surefireDir = new File("surefireDir");
    String suiteName = "suiteName";

    TestScript script = new TestScript(tests, surefireDir, suiteName);
    TestResult testResult = script.handle(result);
    assertTrue(testResult.testsPassed());
    testResult.logOutput(this.log);

    this.control.verify();
  }

  @Test
  public void testHandleTestsFailed() throws MojoExecutionException {
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

    List<File> tests = Arrays.asList(new File("test1"), new File("test2"));
    File surefireDir = new File("surefireDir");
    String suiteName = "suiteName";

    TestScript script = new TestScript(tests, surefireDir, suiteName);
    TestResult testResult = script.handle(result);
    assertFalse(testResult.testsPassed());
    testResult.logOutput(this.log);

    this.control.verify();
  }
}
