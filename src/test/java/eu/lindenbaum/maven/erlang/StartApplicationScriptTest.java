package eu.lindenbaum.maven.erlang;

import static org.easymock.EasyMock.createStrictControl;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.util.Arrays;
import java.util.List;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

import org.apache.maven.plugin.logging.Log;
import org.easymock.IMocksControl;
import org.junit.Before;
import org.junit.Test;

public class StartApplicationScriptTest {
  private IMocksControl control;
  private Log log;

  @Before
  public void setUp() {
    this.control = createStrictControl();
    this.log = this.control.createMock("log", Log.class);
  }

  @Test
  public void testGetHandle() {
    String node = "node";
    List<String> applications = Arrays.asList("application");
    StartApplicationScript script = new StartApplicationScript(node, applications);
    String expression = script.get();
    assertNotNull(expression);
    assertFalse(expression.isEmpty());
    assertFalse(expression.contains("%s"));
  }

  @Test
  public void testHandleSuccess() {
    this.control.replay();

    OtpErlangAtom success = new OtpErlangAtom("ok");
    OtpErlangAtom beforeApp = new OtpErlangAtom("beforeApp");
    OtpErlangList beforeApps = new OtpErlangList(new OtpErlangObject[]{ beforeApp });
    OtpErlangTuple result = new OtpErlangTuple(new OtpErlangObject[]{ success, beforeApps });

    String node = "node";
    List<String> applications = Arrays.asList("application");
    StartApplicationScript script = new StartApplicationScript(node, applications);
    StartResult startResult = script.handle(result);
    startResult.logError(this.log);
    assertTrue(startResult.startSucceeded());
    List<String> apps = startResult.getBeforeApplications();
    assertEquals(1, apps.size());
    assertEquals("beforeApp", apps.get(0));

    this.control.verify();
  }

  @Test
  public void testHandleFailure() {
    this.log.error("{error,what}");

    this.control.replay();

    OtpErlangAtom success = new OtpErlangAtom("error");
    OtpErlangAtom what = new OtpErlangAtom("what");
    OtpErlangTuple error = new OtpErlangTuple(new OtpErlangObject[]{ success, what });
    OtpErlangAtom beforeApp = new OtpErlangAtom("beforeApp");
    OtpErlangList beforeApps = new OtpErlangList(new OtpErlangObject[]{ beforeApp });
    OtpErlangTuple result = new OtpErlangTuple(new OtpErlangObject[]{ error, beforeApps });

    String node = "node";
    List<String> applications = Arrays.asList("application");
    StartApplicationScript script = new StartApplicationScript(node, applications);
    StartResult startResult = script.handle(result);
    startResult.logError(this.log);
    assertFalse(startResult.startSucceeded());
    List<String> apps = startResult.getBeforeApplications();
    assertEquals(1, apps.size());
    assertEquals("beforeApp", apps.get(0));

    this.control.verify();
  }
}
