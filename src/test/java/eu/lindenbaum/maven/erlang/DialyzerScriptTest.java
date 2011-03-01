package eu.lindenbaum.maven.erlang;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.util.Arrays;
import java.util.List;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;

import org.apache.maven.plugin.MojoExecutionException;
import org.junit.Test;

public class DialyzerScriptTest {
  @Test
  public void testGet() throws MojoExecutionException {
    List<File> files = Arrays.asList(new File("file"));
    List<File> includes = Arrays.asList(new File("include"));
    String options = "option";

    DialyzerScript script = new DialyzerScript(files, includes, options);
    String expression = script.get();
    assertNotNull(expression);
    assertFalse(expression.isEmpty());
    assertFalse(expression.contains("%s"));
  }

  @Test
  public void testHandle() throws MojoExecutionException {
    List<File> files = Arrays.asList(new File("file"));
    List<File> includes = Arrays.asList(new File("include"));
    String options = "option1, option2";

    OtpErlangString warning1 = new OtpErlangString("warning1");
    OtpErlangString warning2 = new OtpErlangString("warning2");
    OtpErlangString warning3 = new OtpErlangString("warning3");
    OtpErlangString warning4 = new OtpErlangString("warning4");
    OtpErlangObject[] elems = new OtpErlangObject[]{ warning1, warning2, warning3, warning4 };
    OtpErlangList result = new OtpErlangList(elems);

    DialyzerScript script = new DialyzerScript(files, includes, options);
    String[] converted = script.handle(result);
    assertNotNull(converted);
    assertTrue(converted.length == 4);
    assertEquals("warning1", converted[0]);
    assertEquals("warning2", converted[1]);
    assertEquals("warning3", converted[2]);
    assertEquals("warning4", converted[3]);
  }
}
