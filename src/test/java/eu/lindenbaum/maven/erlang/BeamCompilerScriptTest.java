package eu.lindenbaum.maven.erlang;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;

import java.io.File;
import java.util.Arrays;
import java.util.List;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

import org.apache.maven.plugin.MojoExecutionException;
import org.junit.Test;

public class BeamCompilerScriptTest {
  @Test
  public void testGet() throws MojoExecutionException {
    List<File> files = Arrays.asList(new File("file"));
    List<File> firstFiles = Arrays.asList(new File("file"));
    File outdir = new File("outdir");
    List<File> includes = Arrays.asList(new File("include"));
    List<String> options = Arrays.asList("option");

    BeamCompilerScript script = new BeamCompilerScript(files, firstFiles, outdir, includes, options);
    String expression = script.get();
    assertNotNull(expression);
    assertFalse(expression.isEmpty());
    assertFalse(expression.contains("%s"));
  }

  @Test
  public void testHandle() throws MojoExecutionException {
    List<File> files = Arrays.asList(new File("file"));
    List<File> firstFiles = Arrays.asList(new File("file"));
    File outdir = new File("outdir");
    List<File> includes = Arrays.asList(new File("include"));
    List<String> options = Arrays.asList("option");

    OtpErlangString failed = new OtpErlangString("failed");
    OtpErlangList failedList = new OtpErlangList(new OtpErlangObject[]{ failed });
    OtpErlangString compiled = new OtpErlangString("compiled");
    OtpErlangList compiledList = new OtpErlangList(new OtpErlangObject[]{ compiled });

    OtpErlangString error = new OtpErlangString("error");
    OtpErlangList errorList = new OtpErlangList(new OtpErlangObject[]{ error });
    OtpErlangString warning = new OtpErlangString("warning");
    OtpErlangList warningList = new OtpErlangList(new OtpErlangObject[]{ warning });

    OtpErlangTuple result = new OtpErlangTuple(new OtpErlangObject[]{ failedList, compiledList, errorList,
                                                                     warningList });

    BeamCompilerScript script = new BeamCompilerScript(files, firstFiles, outdir, includes, options);
    CompilerResult compilerResult = script.handle(result);

    assertNotNull(compilerResult);
    assertEquals(1, compilerResult.getFailed().size());
    assertEquals("[failed]", compilerResult.getFailed().toString());
    assertEquals(1, compilerResult.getCompiled().size());
    assertEquals("[compiled]", compilerResult.getCompiled().toString());
    assertEquals(1, compilerResult.getErrors().size());
    assertEquals("[error]", compilerResult.getErrors().toString());
    assertEquals(1, compilerResult.getWarnings().size());
    assertEquals("[warning]", compilerResult.getWarnings().toString());
  }
}
