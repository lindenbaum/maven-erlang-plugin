package eu.lindenbaum.maven.erlang;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

import java.util.Arrays;
import java.util.List;

import com.ericsson.otp.erlang.OtpErlangAtom;

import org.apache.maven.plugin.MojoExecutionException;
import org.junit.Test;

public class StopApplicationScriptTest {
  @Test
  public void testGetHandle() throws MojoExecutionException {
    List<String> applications = Arrays.asList("application");
    StopApplicationScript script = new StopApplicationScript(applications);
    String expression = script.get();
    assertNotNull(expression);
    assertFalse(expression.isEmpty());
    assertFalse(expression.contains("%s"));
  }

  @Test
  public void testHandle() throws MojoExecutionException {
    OtpErlangAtom ignored = new OtpErlangAtom("ignored");

    List<String> applications = Arrays.asList("application");
    StopApplicationScript script = new StopApplicationScript(applications);
    assertNull(script.handle(ignored));
  }
}
