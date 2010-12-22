package eu.lindenbaum.maven.erlang;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

import org.junit.Test;

public class RuntimeInfoScriptTest {
  @Test
  public void testGet() {
    RuntimeInfoScript script = new RuntimeInfoScript();
    String expression = script.get();
    assertNotNull(expression);
    assertFalse(expression.isEmpty());
    assertFalse(expression.contains("%s"));
  }

  @Test
  public void testHandle() {
    OtpErlangString path = new OtpErlangString("/path");
    OtpErlangString version = new OtpErlangString("version");
    OtpErlangTuple result = new OtpErlangTuple(new OtpErlangObject[]{ path, version });

    RuntimeInfoScript script = new RuntimeInfoScript();
    RuntimeInfo info = script.handle(result);
    assertNotNull(info);
    assertEquals("path", info.getLibDirectory().getName());
    assertEquals("version", info.getVersion());
  }
}
