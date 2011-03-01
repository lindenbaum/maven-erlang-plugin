package eu.lindenbaum.maven.erlang;

import static org.easymock.EasyMock.createStrictControl;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.File;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

import org.apache.maven.plugin.logging.Log;
import org.easymock.IMocksControl;
import org.junit.Before;
import org.junit.Test;

public class UploadReleaseScriptTest {
  private IMocksControl control;
  private Log log;

  @Before
  public void setUp() {
    this.control = createStrictControl();
    this.log = this.control.createMock("log", Log.class);
  }

  @Test
  public void testGet() {
    String remoteNode = "node@otherhost.de";
    File release = new File("release.tar.gz");
    UploadReleaseScript script = new UploadReleaseScript(remoteNode, release);
    String expression = script.get();
    assertNotNull(expression);
    assertFalse(expression.isEmpty());
    assertFalse(expression.contains("%s"));
  }

  @Test
  public void testHandleOk() {
    this.control.replay();

    OtpErlangAtom result = new OtpErlangAtom("ok");

    String remoteNode = "node@otherhost.de";
    File release = new File("release.tar.gz");
    UploadReleaseScript script = new UploadReleaseScript(remoteNode, release);
    GenericScriptResult genericScriptResult = script.handle(result);
    assertTrue(genericScriptResult.success());
    genericScriptResult.logOutput(this.log);

    this.control.verify();
  }

  @Test
  public void testHandleError() {
    this.log.error("{error,reason}");

    this.control.replay();

    OtpErlangAtom error = new OtpErlangAtom("error");
    OtpErlangAtom reason = new OtpErlangAtom("reason");
    OtpErlangTuple result = new OtpErlangTuple(new OtpErlangObject[]{ error, reason });

    String remoteNode = "node@otherhost.de";
    File release = new File("release.tar.gz");
    UploadReleaseScript script = new UploadReleaseScript(remoteNode, release);
    GenericScriptResult genericScriptResult = script.handle(result);
    assertFalse(genericScriptResult.success());
    genericScriptResult.logOutput(this.log);

    this.control.verify();
  }
}
