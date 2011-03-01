package eu.lindenbaum.maven.erlang;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;

import java.io.File;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

import org.apache.maven.plugin.MojoExecutionException;
import org.junit.Test;

public class CreateRELEASESScriptTest {
  @Test
  public void testGet() throws MojoExecutionException {
    File rootDir = new File("/rootDir");
    File relFile = new File("/rootDir/release.rel");
    CreateRELEASESScript script = new CreateRELEASESScript(rootDir, relFile);
    String expression = script.get();
    assertNotNull(expression);
    assertFalse(expression.isEmpty());
    assertFalse(expression.contains("%s"));
  }

  @Test
  public void testHandleOk() throws MojoExecutionException {
    OtpErlangAtom result = new OtpErlangAtom("ok");

    File rootDir = new File("/rootDir");
    File relFile = new File("/rootDir/release.rel");
    CreateRELEASESScript script = new CreateRELEASESScript(rootDir, relFile);
    assertEquals("ok", script.handle(result));
  }

  @Test
  public void testHandleError() throws MojoExecutionException {
    OtpErlangAtom error = new OtpErlangAtom("error");
    OtpErlangAtom reason = new OtpErlangAtom("reason");
    OtpErlangTuple result = new OtpErlangTuple(new OtpErlangObject[]{ error, reason });

    File rootDir = new File("/rootDir");
    File relFile = new File("/rootDir/release.rel");
    CreateRELEASESScript script = new CreateRELEASESScript(rootDir, relFile);
    assertEquals("{error,reason}", script.handle(result));
  }
}
