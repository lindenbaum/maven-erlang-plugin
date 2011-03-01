package eu.lindenbaum.maven.erlang;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.File;

import com.ericsson.otp.erlang.OtpErlangAtom;

import org.apache.maven.plugin.MojoExecutionException;
import org.junit.Test;

public class EDocScriptTest {
  @Test
  public void testGet() throws MojoExecutionException {
    String appName = "appName";
    File indir = new File("indir");
    File outdir = new File("outdir");
    File overview = new File("overview.edoc");

    EDocScript script = new EDocScript(appName, indir, outdir, overview);
    String expression = script.get();
    assertNotNull(expression);
    assertFalse(expression.isEmpty());
    assertFalse(expression.contains("%s"));
  }

  @Test
  public void testHandleSuccess() throws MojoExecutionException {
    OtpErlangAtom ignored = new OtpErlangAtom("ok");

    String appName = "appName";
    File indir = new File("indir");
    File outdir = new File("outdir");
    File overview = new File("overview.edoc");

    EDocScript script = new EDocScript(appName, indir, outdir, overview);
    assertTrue(script.handle(ignored));
  }

  @Test
  public void testHandleError() throws MojoExecutionException {
    OtpErlangAtom ignored = new OtpErlangAtom("error");

    String appName = "appName";
    File indir = new File("indir");
    File outdir = new File("outdir");
    File overview = new File("overview.edoc");

    EDocScript script = new EDocScript(appName, indir, outdir, overview);
    assertFalse(script.handle(ignored));
  }
}
