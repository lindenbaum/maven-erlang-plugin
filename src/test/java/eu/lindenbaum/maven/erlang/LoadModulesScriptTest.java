package eu.lindenbaum.maven.erlang;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;

import java.io.File;
import java.util.Arrays;
import java.util.List;

import com.ericsson.otp.erlang.OtpErlangInt;

import org.apache.maven.plugin.MojoExecutionException;
import org.junit.Test;

public class LoadModulesScriptTest {
  @Test
  public void testGet() throws MojoExecutionException {
    List<File> modules = Arrays.asList(new File("module"));
    LoadModulesScript script = new LoadModulesScript(modules);
    String expression = script.get();
    assertNotNull(expression);
    assertFalse(expression.isEmpty());
    assertFalse(expression.contains("%s"));
  }

  @Test
  public void testHandle() throws MojoExecutionException {
    OtpErlangInt result = new OtpErlangInt(2);

    List<File> modules = Arrays.asList(new File("module"));
    LoadModulesScript script = new LoadModulesScript(modules);
    assertEquals(Integer.valueOf(2), script.handle(result));
  }
}
