package eu.lindenbaum.maven.erlang;

import static org.easymock.EasyMock.createStrictControl;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;

import java.io.File;
import java.util.Arrays;
import java.util.List;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

import org.apache.maven.plugin.logging.Log;
import org.easymock.IMocksControl;
import org.junit.Before;
import org.junit.Test;

public class BeamCompilerScriptTest {
  private IMocksControl control;
  private Log log;

  @Before
  public void setup() {
    this.control = createStrictControl();
    this.log = this.control.createMock("log", Log.class);
  }

  @Test
  public void testGet() {
    List<File> files = Arrays.asList(new File("file"));
    File outdir = new File("outdir");
    List<File> includes = Arrays.asList(new File("include"));
    List<String> options = Arrays.asList("option");

    BeamCompilerScript script = new BeamCompilerScript(files, outdir, includes, options);
    String expression = script.get();
    assertNotNull(expression);
    assertFalse(expression.isEmpty());
    assertFalse(expression.contains("%s"));
  }

  @Test
  public void testHandle() {
    this.log.warn("message1");
    this.log.error("message2");

    this.control.replay();

    List<File> files = Arrays.asList(new File("file"));
    File outdir = new File("outdir");
    List<File> includes = Arrays.asList(new File("include"));
    List<String> options = Arrays.asList("option");

    OtpErlangAtom warn = new OtpErlangAtom("warn");
    OtpErlangString string1 = new OtpErlangString("message1");
    OtpErlangTuple message1 = new OtpErlangTuple(new OtpErlangObject[]{ warn, string1 });
    OtpErlangAtom error = new OtpErlangAtom("error");
    OtpErlangString string2 = new OtpErlangString("message2");
    OtpErlangTuple message2 = new OtpErlangTuple(new OtpErlangObject[]{ error, string2 });
    OtpErlangString failed = new OtpErlangString("failed");
    OtpErlangList messages = new OtpErlangList(new OtpErlangObject[]{ message1, message2 });
    OtpErlangTuple result = new OtpErlangTuple(new OtpErlangObject[]{ failed, messages });

    BeamCompilerScript script = new BeamCompilerScript(files, outdir, includes, options);
    CompilerResult compilerResult = script.handle(result);

    assertNotNull(compilerResult);
    assertEquals("failed", compilerResult.getFailed());
    compilerResult.logOutput(this.log);

    this.control.verify();
  }
}
