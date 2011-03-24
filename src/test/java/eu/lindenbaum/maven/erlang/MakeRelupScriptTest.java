package eu.lindenbaum.maven.erlang;

import static org.easymock.EasyMock.createStrictControl;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.util.Arrays;
import java.util.List;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;
import org.easymock.IMocksControl;
import org.junit.Before;
import org.junit.Test;

public class MakeRelupScriptTest {
  private IMocksControl control;
  private Log log;

  @Before
  public void setUp() {
    this.control = createStrictControl();
    this.log = this.control.createMock("log", Log.class);
  }

  @Test
  public void testGet() throws MojoExecutionException {
    File relup = new File("relup");
    File rel = new File("rel.rel");
    List<String> releases = Arrays.asList("other1", "other2");
    List<File> paths = Arrays.asList(new File("path1"), new File("path2"));

    MakeRelupScript script = new MakeRelupScript(relup, rel, releases, paths);
    String expression = script.get();
    assertNotNull(expression);
    assertFalse(expression.isEmpty());
    assertFalse(expression.contains("%s"));
  }

  @Test
  public void testHandle() throws MojoExecutionException {
    this.control.replay();

    OtpErlangList message = new OtpErlangList();
    OtpErlangAtom level = new OtpErlangAtom("ok");
    OtpErlangTuple result = new OtpErlangTuple(new OtpErlangObject[]{ level, message });

    File relup = new File("relup");
    File rel = new File("rel.rel");
    List<String> releases = Arrays.asList("other1", "other2");
    List<File> paths = Arrays.asList(new File("path1"), new File("path2"));
    MakeRelupScript script = new MakeRelupScript(relup, rel, releases, paths);
    GenericScriptResult scriptResult = script.handle(result);
    assertTrue(scriptResult.success());
    scriptResult.logOutput(this.log);

    this.control.verify();
  }

  @Test
  public void testHandleWithWarning() throws MojoExecutionException {
    this.log.warn("message");

    this.control.replay();

    OtpErlangString message = new OtpErlangString("message");
    OtpErlangAtom level = new OtpErlangAtom("ok");
    OtpErlangTuple result = new OtpErlangTuple(new OtpErlangObject[]{ level, message });

    File relup = new File("relup");
    File rel = new File("rel.rel");
    List<String> releases = Arrays.asList("other1", "other2");
    List<File> paths = Arrays.asList(new File("path1"), new File("path2"));
    MakeRelupScript script = new MakeRelupScript(relup, rel, releases, paths);
    GenericScriptResult scriptResult = script.handle(result);
    assertTrue(scriptResult.success());
    scriptResult.logOutput(this.log);

    this.control.verify();
  }

  @Test
  public void testHandleWithError() throws MojoExecutionException {
    this.log.error("message");

    this.control.replay();

    OtpErlangString message = new OtpErlangString("message");
    OtpErlangAtom level = new OtpErlangAtom("error");
    OtpErlangTuple result = new OtpErlangTuple(new OtpErlangObject[]{ level, message });

    File relup = new File("relup");
    File rel = new File("rel.rel");
    List<String> releases = Arrays.asList("other1", "other2");
    List<File> paths = Arrays.asList(new File("path1"), new File("path2"));
    MakeRelupScript script = new MakeRelupScript(relup, rel, releases, paths);
    GenericScriptResult scriptResult = script.handle(result);
    assertFalse(scriptResult.success());
    scriptResult.logOutput(this.log);

    this.control.verify();
  }
}
