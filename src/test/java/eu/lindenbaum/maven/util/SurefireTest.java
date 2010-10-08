package eu.lindenbaum.maven.util;

import static eu.lindenbaum.maven.util.ErlConstants.BEAM_SUFFIX;
import static eu.lindenbaum.maven.util.ErlConstants.ERL;
import static eu.lindenbaum.maven.util.ErlUtils.exec;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugin.logging.SystemStreamLog;
import org.junit.Test;

public class SurefireTest {
  @Test
  public void testSurefire() throws Exception {
    File surefire = new File(getClass().getResource("/surefire.erl").getFile());
    File surefireTest = new File(getClass().getResource("/surefire_test.erl").getFile());

    File surefireBeam = new File(surefire.getName().replace(".erl", BEAM_SUFFIX));
    surefireBeam.deleteOnExit();
    File surefireTestBeam = new File(surefireTest.getName().replace(".erl", BEAM_SUFFIX));
    surefireTestBeam.deleteOnExit();

    List<String> command = new ArrayList<String>();
    command.add(ERL);
    command.add("-eval");
    command.add("c:c(\"" + surefire.getAbsolutePath() + "\", [export_all]).");
    command.add("-eval");
    command.add("c:c(\"" + surefireTest.getAbsolutePath() + "\").");
    command.add("-eval");
    command.add("eunit:test(surefire_test).");
    command.add("-noshell");
    command.add("-s");
    command.add("init");
    command.add("stop");

    String result = exec(command, new SystemStreamLog(), null, new Observer() {
      @Override
      public String handle(int exitValue, String result) throws MojoExecutionException, MojoFailureException {
        if (exitValue != 0) {
          throw new MojoFailureException("test exited with " + exitValue);
        }
        return result;
      }
    });
    if (!result.contains("tests passed.")) {
      throw new Exception("surefire_test did not pass all tests");
    }
  }
}
