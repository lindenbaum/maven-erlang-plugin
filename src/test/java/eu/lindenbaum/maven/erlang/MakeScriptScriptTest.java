package eu.lindenbaum.maven.erlang;

import static org.easymock.EasyMock.createStrictControl;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.File;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

import org.apache.maven.plugin.logging.Log;
import org.easymock.IMocksControl;
import org.junit.Before;
import org.junit.Test;

public class MakeScriptScriptTest {
  private IMocksControl control;
  private Log log;

  @Before
  public void setUp() {
    this.control = createStrictControl();
    this.log = this.control.createMock("log", Log.class);
  }

  @Test
  public void testGet() {
    File outdir = new File("outdir");
    File relFile = new File("relfile.rel");
    MakeScriptScript script = new MakeScriptScript(relFile, outdir, null);
    String expression = script.get();
    assertNotNull(expression);
    assertFalse(expression.isEmpty());
    assertFalse(expression.contains("%s"));
  }

  @Test
  public void testHandle() {
    this.control.replay();

    OtpErlangString message = new OtpErlangString("ignored");
    OtpErlangAtom level = new OtpErlangAtom("ok");
    OtpErlangTuple result = new OtpErlangTuple(new OtpErlangObject[]{ level, message });

    File outdir = new File("outdir");
    File relFile = new File("relfile.rel");
    MakeScriptScript script = new MakeScriptScript(relFile, outdir, null);
    SystoolsScriptResult scriptResult = script.handle(result);
    assertTrue(scriptResult.success());
    scriptResult.logOutput(this.log);

    this.control.verify();
  }

  @Test
  public void testHandleWithWarning() {
    this.log.warn("message");

    this.control.replay();

    OtpErlangString message = new OtpErlangString("message");
    OtpErlangAtom level = new OtpErlangAtom("warn");
    OtpErlangTuple result = new OtpErlangTuple(new OtpErlangObject[]{ level, message });

    File outdir = new File("outdir");
    File relFile = new File("relfile.rel");
    MakeScriptScript script = new MakeScriptScript(relFile, outdir, null);
    SystoolsScriptResult scriptResult = script.handle(result);
    assertTrue(scriptResult.success());
    scriptResult.logOutput(this.log);

    this.control.verify();
  }

  @Test
  public void testHandleWithError() {
    this.log.error("message");

    this.control.replay();

    OtpErlangString message = new OtpErlangString("message");
    OtpErlangAtom level = new OtpErlangAtom("error");
    OtpErlangTuple result = new OtpErlangTuple(new OtpErlangObject[]{ level, message });

    File outdir = new File("outdir");
    File relFile = new File("relfile.rel");
    MakeScriptScript script = new MakeScriptScript(relFile, outdir, null);
    SystoolsScriptResult scriptResult = script.handle(result);
    assertFalse(scriptResult.success());
    scriptResult.logOutput(this.log);

    this.control.verify();
  }
}
