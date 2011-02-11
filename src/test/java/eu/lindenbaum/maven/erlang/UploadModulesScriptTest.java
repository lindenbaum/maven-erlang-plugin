package eu.lindenbaum.maven.erlang;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;

import java.io.File;
import java.util.Arrays;
import java.util.List;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

import org.junit.Test;

public class UploadModulesScriptTest {
  @Test
  public void testGet() {
    String remoteNode = "node@otherhost.de";
    List<File> modules = Arrays.asList(new File("module"));
    UploadModulesScript script = new UploadModulesScript(remoteNode, modules);
    String expression = script.get();
    assertNotNull(expression);
    assertFalse(expression.isEmpty());
    assertFalse(expression.contains("%s"));
  }

  @Test
  public void testHandleOk() {
    OtpErlangAtom result = new OtpErlangAtom("ok");

    String remoteNode = "node@otherhost.de";
    List<File> modules = Arrays.asList(new File("module"));
    UploadModulesScript script = new UploadModulesScript(remoteNode, modules);
    assertEquals("ok", script.handle(result));
  }

  @Test
  public void testHandleError() {
    OtpErlangAtom error = new OtpErlangAtom("error");
    OtpErlangAtom reason = new OtpErlangAtom("reason");
    OtpErlangTuple result = new OtpErlangTuple(new OtpErlangObject[]{ error, reason });

    String remoteNode = "node@otherhost.de";
    List<File> modules = Arrays.asList(new File("module"));
    UploadModulesScript script = new UploadModulesScript(remoteNode, modules);
    assertEquals("{error,reason}", script.handle(result));
  }
}
