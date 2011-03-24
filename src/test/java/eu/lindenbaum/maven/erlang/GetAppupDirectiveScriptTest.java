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

public class GetAppupDirectiveScriptTest {
  @Test
  public void testGet() throws MojoExecutionException {
    String module = "module";
    File path = new File("/path");

    GetAppupDirectiveScript script = new GetAppupDirectiveScript(module, path);
    String expression = script.get();
    assertNotNull(expression);
    assertFalse(expression.isEmpty());
    assertFalse(expression.contains("%s"));
  }

  @Test
  public void testHandleEmpty() throws MojoExecutionException {
    String module = "module";
    File path = new File("/path");

    OtpErlangAtom result = new OtpErlangAtom("error");

    GetAppupDirectiveScript script = new GetAppupDirectiveScript(module, path);
    String directive = script.handle(result);
    assertEquals(null, directive);
  }

  @Test
  public void testHandleNotEmpty() throws MojoExecutionException {
    String module = "module";
    File path = new File("/path");

    OtpErlangAtom first = new OtpErlangAtom("update");
    OtpErlangAtom second = new OtpErlangAtom("module");
    OtpErlangTuple result = new OtpErlangTuple(new OtpErlangObject[]{ first, second });

    GetAppupDirectiveScript script = new GetAppupDirectiveScript(module, path);
    String directive = script.handle(result);
    assertEquals("{update,module}", directive);
  }
}
