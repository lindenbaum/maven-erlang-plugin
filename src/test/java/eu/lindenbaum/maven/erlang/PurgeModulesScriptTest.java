package eu.lindenbaum.maven.erlang;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

import com.ericsson.otp.erlang.OtpErlangAtom;

import org.apache.maven.plugin.MojoExecutionException;
import org.junit.Test;

public class PurgeModulesScriptTest {
  @Test
  public void testGet() throws MojoExecutionException {
    PurgeModulesScript script = new PurgeModulesScript();
    String expression = script.get();
    assertNotNull(expression);
    assertFalse(expression.isEmpty());
    assertFalse(expression.contains("%s"));
  }

  @Test
  public void testHandle() throws MojoExecutionException {
    OtpErlangAtom ignored = new OtpErlangAtom("ignored");

    PurgeModulesScript script = new PurgeModulesScript();
    assertNull(script.handle(ignored));
  }
}
