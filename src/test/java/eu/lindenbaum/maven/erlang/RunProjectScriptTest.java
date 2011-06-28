package eu.lindenbaum.maven.erlang;

import static org.easymock.EasyMock.createStrictControl;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.util.Arrays;
import java.util.List;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;
import org.easymock.IMocksControl;
import org.junit.Before;
import org.junit.Test;

public class RunProjectScriptTest {
  private IMocksControl control;
  private Log log;

  @Before
  public void setUp() {
    this.control = createStrictControl();
    this.log = this.control.createMock("log", Log.class);
  }

  @Test
  public void testGetHandle() throws MojoExecutionException {
    String node = "node";
    List<String> applications = Arrays.asList("application");
    RunProjectScript script = new RunProjectScript(node, applications);
    String expression = script.get();
    assertNotNull(expression);
    assertFalse(expression.isEmpty());
    assertFalse(expression.contains("%s"));
  }

  @Test
  public void testHandleSuccess() throws MojoExecutionException {
    this.log.info("Applications successfully started:");
    this.log.info(" * application");
    this.log.info("");

    this.control.replay();

    OtpErlangAtom application = new OtpErlangAtom("application");
    OtpErlangList successList = new OtpErlangList(new OtpErlangObject[]{ application });
    OtpErlangList failedList = new OtpErlangList();

    OtpErlangTuple result = new OtpErlangTuple(new OtpErlangObject[]{ successList, failedList });

    String node = "node";
    List<String> applications = Arrays.asList("application");
    RunProjectScript script = new RunProjectScript(node, applications);
    GenericScriptResult startResult = script.handle(result);
    startResult.logOutput(this.log);
    assertTrue(startResult.success());

    this.control.verify();
  }

  @Test
  public void testHandleFailure() throws MojoExecutionException {
    this.log.error("Applications that could not be started:");
    this.log.error(" * {application,{error,what}}");

    this.control.replay();

    OtpErlangAtom application = new OtpErlangAtom("application");

    OtpErlangAtom error = new OtpErlangAtom("error");
    OtpErlangAtom what = new OtpErlangAtom("what");
    OtpErlangTuple errorTuple = new OtpErlangTuple(new OtpErlangObject[]{ error, what });
    OtpErlangTuple failedTuple = new OtpErlangTuple(new OtpErlangObject[]{ application, errorTuple });

    OtpErlangList successList = new OtpErlangList();
    OtpErlangList failedList = new OtpErlangList(new OtpErlangObject[]{ failedTuple });

    OtpErlangTuple result = new OtpErlangTuple(new OtpErlangObject[]{ successList, failedList });

    String node = "node";
    List<String> applications = Arrays.asList("application");
    RunProjectScript script = new RunProjectScript(node, applications);
    GenericScriptResult startResult = script.handle(result);
    startResult.logOutput(this.log);
    assertFalse(startResult.success());

    this.control.verify();
  }
}
