package eu.lindenbaum.maven.erlang;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.File;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

import org.apache.maven.plugin.MojoExecutionException;
import org.junit.Test;

public class CheckAppScriptTest {
  @Test
  public void testGet() throws MojoExecutionException {
    File appFile = new File("appFile");

    CheckAppScript script = new CheckAppScript(appFile);
    String expression = script.get();
    assertNotNull(expression);
    assertFalse(expression.isEmpty());
    assertFalse(expression.contains("%s"));
  }

  @Test
  public void testHandleDefault() throws MojoExecutionException {
    File appFile = new File("appFile");

    OtpErlangAtom error = new OtpErlangAtom("error");
    OtpErlangAtom undef = new OtpErlangAtom("undefined");
    OtpErlangList nil = new OtpErlangList();
    OtpErlangTuple result = new OtpErlangTuple(new OtpErlangObject[]{ error, undef, undef, undef, nil, nil });

    CheckAppScript script = new CheckAppScript(appFile);
    CheckAppResult appResult = script.handle(result);
    assertFalse(appResult.success());
    assertEquals("undefined", appResult.getName());
    assertEquals("undefined", appResult.getVersion());
    assertEquals("undefined", appResult.getStartModule());
    assertTrue(appResult.getModules().isEmpty());
    assertTrue(appResult.getApplications().isEmpty());
  }

  @Test
  public void testHandle() throws MojoExecutionException {
    File appFile = new File("appFile");

    OtpErlangAtom ok = new OtpErlangAtom("ok");
    OtpErlangAtom name = new OtpErlangAtom("name");
    OtpErlangString version = new OtpErlangString("1.0.0-SNAPSHOT");
    OtpErlangAtom startModule = new OtpErlangAtom("startModule");
    OtpErlangList modules = new OtpErlangList(new OtpErlangObject[]{ new OtpErlangAtom("module1"),
                                                                    new OtpErlangAtom("Module2") });
    OtpErlangList applications = new OtpErlangList(new OtpErlangObject[]{ new OtpErlangAtom("sasl") });
    OtpErlangTuple result = new OtpErlangTuple(new OtpErlangObject[]{ ok, name, version, startModule,
                                                                     modules, applications });

    CheckAppScript script = new CheckAppScript(appFile);
    CheckAppResult appResult = script.handle(result);
    assertTrue(appResult.success());
    assertEquals("name", appResult.getName());
    assertEquals("1.0.0-SNAPSHOT", appResult.getVersion());
    assertEquals("startModule", appResult.getStartModule());
    assertEquals("module1", appResult.getModules().get(0));
    assertEquals("Module2", appResult.getModules().get(1));
    assertEquals("sasl", appResult.getApplications().get(0));
  }
}
