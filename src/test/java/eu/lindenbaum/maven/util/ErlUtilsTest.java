package eu.lindenbaum.maven.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;

import java.util.Arrays;
import java.util.List;

import org.apache.maven.plugin.logging.Log;
import org.apache.maven.plugin.logging.SystemStreamLog;
import org.junit.Test;

public class ErlUtilsTest {
  private static final Log log = new SystemStreamLog();

  private static final String DIR_COMMAND = "dir";
  private static final String LS_COMMAND = "ls";

  @Test
  public void testExec() throws Exception {
    String dir = getSystemSpecificListCommand();
    List<String> command = Arrays.asList(new String[]{ dir });
    assertEquals("ok", ErlUtils.exec(command, log, null, new ProcessListener() {
      @Override
      public String processCompleted(int exitValue, List<String> processOutput) {
        assertEquals(exitValue, 0);
        assertFalse(processOutput.isEmpty());
        return "ok";
      }
    }));
  }

  private String getSystemSpecificListCommand() {
    if ("Mac OS X".equals(System.getProperty("os.name"))) {
      return LS_COMMAND;
    }
    else {
      return DIR_COMMAND;
    }
  }

  @Test
  public void testEval2() throws Exception {
    assertEquals("ok", ErlUtils.eval(log, "io:format(\"ok~n\")"));
  }

  @Test
  public void testEval3() throws Exception {
    assertEquals("ok", ErlUtils.eval(log, "io:format(\"ok~n\")", null));
  }

  @Test
  public void testEval4() throws Exception {
    assertEquals("ok", ErlUtils.eval(log, "io:format(\"ok~n\")", null, null));
  }
}
