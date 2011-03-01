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

public class UploadScriptTest {
  private IMocksControl control;
  private Log log;

  @Before
  public void setUp() {
    this.control = createStrictControl();
    this.log = this.control.createMock("log", Log.class);
  }

  @Test
  public void testGet() throws MojoExecutionException {
    String remoteNode = "node@otherhost.de";
    List<File> modules = Arrays.asList(new File("module"));
    List<File> appFiles = Arrays.asList(new File("appFile"));
    UploadScript script = new UploadScript(remoteNode, modules, appFiles);
    String expression = script.get();
    assertNotNull(expression);
    assertFalse(expression.isEmpty());
    assertFalse(expression.contains("%s"));
  }

  @Test
  public void testHandleOk() throws MojoExecutionException {
    this.log.info("[\"module\"]");

    this.control.replay();

    OtpErlangString module = new OtpErlangString("module");
    OtpErlangList result = new OtpErlangList(new OtpErlangObject[]{ module });

    String remoteNode = "node@otherhost.de";
    List<File> modules = Arrays.asList(new File("module"));
    List<File> appFiles = Arrays.asList(new File("appFile"));
    UploadScript script = new UploadScript(remoteNode, modules, appFiles);
    GenericScriptResult genericScriptResult = script.handle(result);
    assertTrue(genericScriptResult.success());
    genericScriptResult.logOutput(this.log);

    this.control.verify();
  }

  @Test
  public void testHandleError() throws MojoExecutionException {
    this.log.error("{error,reason}");

    this.control.replay();

    OtpErlangAtom error = new OtpErlangAtom("error");
    OtpErlangAtom reason = new OtpErlangAtom("reason");
    OtpErlangTuple result = new OtpErlangTuple(new OtpErlangObject[]{ error, reason });

    String remoteNode = "node@otherhost.de";
    List<File> modules = Arrays.asList(new File("module"));
    List<File> appFiles = Arrays.asList(new File("appFile"));
    UploadScript script = new UploadScript(remoteNode, modules, appFiles);
    GenericScriptResult genericScriptResult = script.handle(result);
    assertFalse(genericScriptResult.success());
    genericScriptResult.logOutput(this.log);

    this.control.verify();
  }
}
