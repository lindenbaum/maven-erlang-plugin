package eu.lindenbaum.maven.erlang;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.util.Map;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

import org.junit.Test;

public class CheckRelScriptTest {
  @Test
  public void testGet() {
    File relFile = new File("relFile");

    CheckRelScript script = new CheckRelScript(relFile);
    String expression = script.get();
    assertNotNull(expression);
    assertFalse(expression.isEmpty());
    assertFalse(expression.contains("%s"));
  }

  @Test
  public void testHandleDefault() {
    File relFile = new File("relFile");

    OtpErlangAtom error = new OtpErlangAtom("error");
    OtpErlangAtom undef = new OtpErlangAtom("undefined");
    OtpErlangList nil = new OtpErlangList();
    OtpErlangTuple result = new OtpErlangTuple(new OtpErlangObject[]{ error, undef, undef, undef, nil });

    CheckRelScript script = new CheckRelScript(relFile);
    CheckRelResult relResult = script.handle(result);
    assertEquals("undefined", relResult.getName());
    assertEquals("undefined", relResult.getReleaseVersion());
    assertEquals("undefined", relResult.getErtsVersion());
    assertTrue(relResult.getApplications().isEmpty());
  }

  @Test
  public void testHandle() {
    File relFile = new File("relFile");

    OtpErlangAtom ok = new OtpErlangAtom("ok");
    OtpErlangAtom name = new OtpErlangAtom("name");
    OtpErlangString releaseVersion = new OtpErlangString("1.0.0-SNAPSHOT");
    OtpErlangAtom ertsVersion = new OtpErlangAtom("5.8");

    OtpErlangTuple app1 = new OtpErlangTuple(new OtpErlangObject[]{ new OtpErlangAtom("sasl"),
                                                                   new OtpErlangString("1.0") });
    OtpErlangTuple app2 = new OtpErlangTuple(new OtpErlangObject[]{ new OtpErlangAtom("mnesia"),
                                                                   new OtpErlangString("2.0") });
    OtpErlangTuple app3 = new OtpErlangTuple(new OtpErlangObject[]{ new OtpErlangAtom("custom"),
                                                                   new OtpErlangString("3.0") });
    OtpErlangList applications = new OtpErlangList(new OtpErlangObject[]{ app1, app2, app3 });
    OtpErlangTuple result = new OtpErlangTuple(new OtpErlangObject[]{ ok, name, releaseVersion, ertsVersion,
                                                                     applications });

    CheckRelScript script = new CheckRelScript(relFile);
    CheckRelResult relResult = script.handle(result);
    assertEquals("name", relResult.getName());
    assertEquals("1.0.0-SNAPSHOT", relResult.getReleaseVersion());
    assertEquals("5.8", relResult.getErtsVersion());
    Map<String, String> apps = relResult.getApplications();
    assertEquals("1.0", apps.get("sasl"));
    assertEquals("2.0", apps.get("mnesia"));
    assertEquals("3.0", apps.get("custom"));
    assertEquals(3, apps.size());
  }
}
