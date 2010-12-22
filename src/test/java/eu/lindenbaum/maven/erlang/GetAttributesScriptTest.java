package eu.lindenbaum.maven.erlang;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;

import java.io.File;
import java.util.Arrays;
import java.util.List;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;

import org.junit.Test;

public class GetAttributesScriptTest {
  @Test
  public void testGet() {
    List<File> modules = Arrays.asList(new File("file1.erl", "file2.beam"));
    String attribute = "attribute";

    GetAttributesScript script = new GetAttributesScript(modules, attribute);
    String expression = script.get();
    assertNotNull(expression);
    assertFalse(expression.isEmpty());
    assertFalse(expression.contains("%s"));
  }

  @Test
  public void testHandleEmpty() {
    List<File> modules = Arrays.asList(new File("file1.erl", "file2.beam"));
    String attribute = "attribute";

    OtpErlangList result = new OtpErlangList();

    GetAttributesScript script = new GetAttributesScript(modules, attribute);
    String filtered = script.handle(result);
    assertEquals("[]", filtered);
  }

  @Test
  public void testHandleNotEmpty() {
    List<File> modules = Arrays.asList(new File("file1.erl", "file2.beam"));
    String attribute = "attribute";

    OtpErlangAtom file1 = new OtpErlangAtom("value1");
    OtpErlangAtom file2 = new OtpErlangAtom("value2");
    OtpErlangList result = new OtpErlangList(new OtpErlangObject[]{ file1, file2 });

    GetAttributesScript script = new GetAttributesScript(modules, attribute);
    String filtered = script.handle(result);
    assertEquals("['value1', 'value2']", filtered);
  }
}
