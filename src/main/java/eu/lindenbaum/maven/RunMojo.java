package eu.lindenbaum.maven;

import static eu.lindenbaum.maven.util.ErlConstants.ARTIFACT_TYPE_OTP;
import static eu.lindenbaum.maven.util.ErlConstants.ERL;
import static eu.lindenbaum.maven.util.FileUtils.getDependencies;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.List;

import org.apache.maven.plugin.Mojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugin.logging.Log;

/**
 * <p>
 * This {@link Mojo} runs a, {@code erlang-otp} packaged project interactively
 * in a shell. Therefore an erlang shell with {@code sasl} is started and the
 * application is started using {@code application:start/1}. To package and
 * startthe application simply use
 * 
 * <pre>
 * mvn erlang:run
 * </pre>
 * </p>
 * <p>
 * BUG It is known that control characters cannot be used in the interactive
 * shell.
 * </p>
 * <p>
 * TODO Running release projects is currently not supported, but could be useful
 * to be implemented in the future.
 * </p>
 * 
 * @goal run
 * @execute phase="package"
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 */
public final class RunMojo extends AbstractErlangMojo {
  @Override
  public void execute() throws MojoExecutionException, MojoFailureException {
    Log log = getLog();
    if (ARTIFACT_TYPE_OTP.equals(this.project.getPackaging())) {
      ArrayList<String> command = new ArrayList<String>();
      command.add(ERL);
      command.add("-boot");
      command.add("start_sasl");
      for (File lib : getDependencies(this.targetLib)) {
        command.add("-pa");
        command.add(lib.getAbsolutePath());
      }

      String afterStarted = "application:start(\'" + this.project.getArtifactId() + "\').\n";
      runInteractive(log, command, afterStarted, this.targetEbin);
    }
    else {
      log.warn("Can only run projects with " + ARTIFACT_TYPE_OTP);
    }
  }

  /**
   * Executes the given command using a {@link ProcessBuilder} and blocks until
   * the command completed. All input from stdin will be redirected into the
   * process while the process output is mirrored onto stdout.
   * 
   * @param log logger to use
   * @param command command to run interactively
   * @param afterStarted command to give to the process when started
   * @param cwd current working directory for the process to start
   */
  private static void runInteractive(Log log, List<String> command, final String afterStarted, File cwd) {
    ProcessBuilder processBuilder = new ProcessBuilder(command);
    processBuilder.directory(cwd);
    processBuilder.redirectErrorStream(true);
    log.debug("Working directory " + processBuilder.directory());
    try {
      Process process = processBuilder.start();
      final OutputStream sysWriter = System.out;
      final InputStream processReader = process.getInputStream();
      Thread output = new Thread(new Runnable() {
        @Override
        public void run() {
          try {
            int character;
            while ((character = processReader.read()) != -1) {
              sysWriter.write((char) character);
              sysWriter.flush();
            }
          }
          catch (IOException e) {
            // ignored
          }
        }
      });
      final InputStream sysReader = System.in;
      final OutputStream processWriter = process.getOutputStream();
      Thread input = new Thread(new Runnable() {
        @Override
        public void run() {
          try {
            processWriter.write(afterStarted.getBytes());
            processWriter.flush();
            int character;
            while ((character = sysReader.read()) != -1) {
              processWriter.write(character);
              processWriter.flush();
            }
          }
          catch (IOException e) {
            // ignored
          }
        }
      });
      output.start();
      input.start();
      process.waitFor();
      output.join();
      input.interrupt();
      sysWriter.write('\n');
      sysWriter.flush();
    }
    catch (Exception e) {
      log.error(e.getMessage(), e);
    }
  }
}
