package eu.lindenbaum.maven.erlang;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

import java.io.File;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangString;

import org.junit.Test;

public class CheckAppUpScriptTest {
  @Test
  public void testGet() {
    File appFile = new File("appUpFile");
    String version = "version";

    CheckAppUpScript script = new CheckAppUpScript(appFile, version);
    String expression = script.get();
    assertNotNull(expression);
    assertFalse(expression.isEmpty());
    assertFalse(expression.contains("%s"));
  }

  @Test
  public void testHandleNoError() {
    File appFile = new File("appFile");
    String version = "version";

    OtpErlangList result = new OtpErlangList();

    CheckAppUpScript script = new CheckAppUpScript(appFile, version);
    assertNull(script.handle(result));
  }

  @Test
  public void testHandleWithError() {
    File appFile = new File("appFile");
    String version = "version";

    OtpErlangString result = new OtpErlangString("some error");

    CheckAppUpScript script = new CheckAppUpScript(appFile, version);
    assertEquals("some error", script.handle(result));
  }
}
