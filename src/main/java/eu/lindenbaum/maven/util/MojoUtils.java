package eu.lindenbaum.maven.util;

import static eu.lindenbaum.maven.util.CollectionUtils.filter;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import eu.lindenbaum.maven.erlang.DialyzerScript;
import eu.lindenbaum.maven.erlang.MavenSelf;
import eu.lindenbaum.maven.erlang.NodeShutdownHook;
import eu.lindenbaum.maven.erlang.Script;

import com.ericsson.otp.erlang.OtpAuthException;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpPeer;
import com.ericsson.otp.erlang.OtpSelf;

import org.apache.maven.plugin.Mojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;

/**
 * Containing utilities related to maven-erlang-plugin specific {@link Mojo}s.
 * 
 * @author Olle Törnström <olle.toernstroem@lindenbaum.eu>
 * @author Sven Heyll <sven.heyll@gmail.com>
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 * @since 2.0.0
 */
public final class MojoUtils {
  private static final String WINDOWS = "WINDOWS";
  private static final String EUNIT_TESTS_SUFFIX = "_tests" + ErlConstants.BEAM_SUFFIX;
  private static final Pattern DIALYZER_WARNING = Pattern.compile("(.+):(\\d+): (.+)");

  /**
   * Returns whether the executing JVM is running under Microsoft Windows or
   * not.
   * 
   * @return {@code true} on Microsoft Windows systems, {@code false} otherwise
   * @throws MojoExecutionException in case the 'os.name' property is not set
   */
  public static boolean isWindows() throws MojoExecutionException {
    String property = System.getProperty("os.name");
    if (property != null) {
      if (property.toUpperCase().contains(WINDOWS)) {
        return true;
      }
    }
    else {
      throw new MojoExecutionException("Java property 'os.name' not set.");
    }
    return false;
  }

  /**
   * Attaches the plugin to a backend erlang node. If the backend node is not
   * already running it will be started.
   * 
   * @param log logger to use
   * @param cmd path to the {@code erl} command
   * @param nodeName name of the backend to connect to
   * @param nodeCookie cookie of the backend to connect to
   * @param cwd the backend node's current working directory
   * @param backendLog file to write the ouput from the backend nodes to
   * @throws MojoExecutionException
   */
  public static void startBackend(final Log log,
                                  String cmd,
                                  String nodeName,
                                  String nodeCookie,
                                  final File cwd,
                                  final File backendLog) throws MojoExecutionException {
    OtpPeer peer = new OtpPeer(nodeName);
    try {
      try {
        String startupName = "maven-erlang-plugin-startup-" + System.nanoTime();
        OtpSelf self = nodeCookie != null ? new OtpSelf(startupName, nodeCookie) : new OtpSelf(startupName);
        self.connect(peer);
        log.debug("Node " + peer + " is already running.");
      }
      catch (IOException e) {
        log.debug("starting " + peer + ".");
        ArrayList<String> command = new ArrayList<String>();
        command.add(cmd);
        command.add("-boot");
        command.add("start_sasl");
        command.add("-name");
        command.add(peer.node());
        command.add("-noshell");
        if (nodeCookie != null) {
          command.add("-setcookie");
          command.add(nodeCookie);
        }

        ProcessBuilder processBuilder = new ProcessBuilder(command);
        processBuilder.redirectErrorStream(true);
        final Process process = processBuilder.start();

        // write node output to log file
        Thread thread = new Thread(new Runnable() {
          @Override
          public void run() {
            InputStreamReader input = new InputStreamReader(process.getInputStream());
            BufferedReader reader = new BufferedReader(input);
            try {
              PrintWriter writer = new PrintWriter(new FileWriter(backendLog, true));
              String line = null;
              try {
                while ((line = reader.readLine()) != null) {
                  writer.println(line);
                }
              }
              catch (IOException e) {
                writer.println("Failed to read node output: " + e);
              }
              finally {
                writer.flush();
                writer.close();
              }
            }
            catch (Exception e) {
              log.warn("Unable to write backend node log file " + backendLog, e);
            }
          }
        });
        thread.setDaemon(true);
        thread.start();
        log.debug("Node " + peer + " sucessfully started.");

        MavenSelf.get(nodeCookie).exec(nodeName, new Script<Void>() {
          @Override
          public String get() {
            return "ok = file:set_cwd(\"" + cwd.getAbsolutePath() + "\").";
          }

          @Override
          public Void handle(OtpErlangObject result) {
            return null;
          }
        });
      }
      try {
        Runtime.getRuntime().addShutdownHook(NodeShutdownHook.get(nodeName, nodeCookie));
      }
      catch (IllegalArgumentException e1) {
        log.debug("shutdown hook already registered.");
      }
    }
    catch (IOException e) {
      throw new MojoExecutionException("Failed to start " + peer + ".", e);
    }
    catch (OtpAuthException e) {
      throw new MojoExecutionException("Failed to connect to " + peer + ".", e);
    }
  }

  /**
   * Returns whether there are erlang source/header files in a specific
   * directory (recursive) having a newer modification time than a given
   * reference file.
   * 
   * @param reference the file taken as reference time (modified)
   * @param dirs input directories to check
   * @return true if there are .erl/.hrl files newer than the reference file
   */
  public static boolean newerFilesThan(File reference, Collection<File> dirs) {
    final long referenceTime = reference.lastModified();
    if (referenceTime > 0L) {
      List<File> sources = new ArrayList<File>();
      for (File dir : dirs) {
        sources.addAll(FileUtils.getFilesRecursive(dir, ErlConstants.ERL_SUFFIX));
        sources.addAll(FileUtils.getFilesRecursive(dir, ErlConstants.HRL_SUFFIX));
      }
      for (File file : sources) {
        if (file.lastModified() > referenceTime) {
          return true;
        }
      }
      return false;
    }
    return true;
  }

  /**
   * Parses the output of the {@link DialyzerScript} and converts it to a
   * beautified {@link Collection} of strings to be logged.
   * 
   * @param output the dialyzers warning output
   * @param sources a complete collection of source files dialyzer was run on
   * @return a list of warning strings that can be logged
   */
  public static Collection<String> parseDialyzerOutput(String[] output, Collection<File> sources) {
    ArrayList<String> warnings = new ArrayList<String>();
    for (String warning : output) {
      Matcher matcher = DIALYZER_WARNING.matcher(warning);
      if (matcher.matches()) {
        String fileName = matcher.group(1);
        File file = FileUtils.getFile(fileName, sources);
        if (file != null) {
          warnings.add(" * " + file + ":" + matcher.group(2));
        }
        else {
          warnings.add(" * " + fileName + ":" + matcher.group(2));
        }
        warnings.add("   " + matcher.group(3));
      }
      else {
        warnings.add(" * " + warning);
      }
    }
    return warnings;
  }

  /**
   * Filters a given list of compiled test files and returns a list of files
   * that must be run (using eunit) to cover all modules that could possibly
   * contain tests. In fact this will remove a module ending with
   * <code>_tests.beam</code> from the list whenever another module is contained
   * without this suffix (the according test will be run by eunit
   * automatically). The input list will not be modified.
   * 
   * @param tests list of test modules to find the tests to execute for.
   * @param excludes list of infrastructure modules that should be excluded from
   *          test execution
   * @return a non-{@code null} list of test modules
   */
  public static List<File> getEunitTestSet(final Collection<File> tests, Collection<File> excludes) {
    ArrayList<File> prefiltered = new ArrayList<File>(tests);
    prefiltered.removeAll(excludes);
    return new ArrayList<File>(filter(new Predicate<File>() {
      @Override
      public boolean pred(File object) {
        String testPath = object.getAbsolutePath();
        if (testPath.endsWith(EUNIT_TESTS_SUFFIX)) {
          String impl = testPath.replace(EUNIT_TESTS_SUFFIX, ErlConstants.BEAM_SUFFIX);
          return !tests.contains(new File(impl));
        }
        return true;
      }
    }, prefiltered));
  }

  /**
   * Emits an info that backend node output is available in the given file if
   * existing.
   * 
   * @param log logger to use
   * @param backendLog referring to the backend log file.
   */
  public static void emitBackendLogInfo(Log log, File backendLog) {
    if (backendLog.isFile()) {
      log.info("");
      log.info("The erlang backend node output is available in:");
      log.info(backendLog.toString());
    }
  }
}
