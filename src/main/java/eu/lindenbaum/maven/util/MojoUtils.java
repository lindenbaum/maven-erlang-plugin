package eu.lindenbaum.maven.util;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import eu.lindenbaum.maven.Properties;
import eu.lindenbaum.maven.erlang.NodeShutdownHook;

import com.ericsson.otp.erlang.OtpAuthException;
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

  /**
   * Returns a list of script files used by the plugin to provide a proper test
   * phase, e.g. erlang scripts for surefire reports.
   * 
   * @param p the properties object containing plug-in paths
   * @return a non-{@code null} collection of test support scripts
   */
  public static Collection<File> getTestSupportScripts(Properties p) {
    Collection<File> supportFiles = new ArrayList<File>();
    supportFiles.add(new File(p.targetTestEbin(), "surefire.erl"));
    supportFiles.add(new File(p.targetTestEbin(), "cover2.erl"));
    supportFiles.add(new File(p.targetTestEbin(), "ttycapture.erl"));
    return supportFiles;
  }

  /**
   * Returns a list of artifacts used by the plugin to provide a proper test
   * phase, e.g. the compiled artifacts of
   * {@link #getTestSupportScripts(Properties)}.
   * 
   * @param p the properties object containing plug-in paths
   * @return a non-{@code null} collection of test support artifacts
   */
  public static Collection<File> getTestSupportArtifacts(Properties p) {
    Collection<File> supportArtifacts = new ArrayList<File>();
    for (File script : getTestSupportScripts(p)) {
      String erl = ErlConstants.ERL_SUFFIX;
      String beam = ErlConstants.BEAM_SUFFIX;
      supportArtifacts.add(new File(script.getAbsolutePath().replace(erl, beam)));
    }
    return supportArtifacts;
  }

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
   * @throws MojoExecutionException
   */
  public static void startBackend(Log log, String cmd, String nodeName, String nodeCookie) throws MojoExecutionException {
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
        command.add("-detached");
        if (nodeCookie != null) {
          command.add("-setcookie");
          command.add(nodeCookie);
        }
        Process process = new ProcessBuilder(command).start();
        if (process.waitFor() != 0) {
          throw new MojoExecutionException("Failed to start " + peer + ".");
        }
        log.debug("Node " + peer + " sucessfully started.");
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
    catch (InterruptedException e) {
      throw new MojoExecutionException("Failed to start " + peer + ".", e);
    }
  }

  /**
   * Returns whether there are erlang source/header files in a specific
   * directory (recursive) having a newer modification time than a given
   * reference file.
   * 
   * @param dir input directory to check
   * @param reference the file taken as reference time (modified)
   * @return true if there are .erl/.hrl files newer than the reference file
   */
  public static boolean newerFilesThan(File dir, File reference) {
    final long referenceTime = reference.lastModified();
    if (referenceTime > 0L) {
      List<File> sources = FileUtils.getFilesRecursive(dir, ErlConstants.ERL_SUFFIX);
      sources.addAll(FileUtils.getFilesRecursive(dir, ErlConstants.HRL_SUFFIX));
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
   * Gathers a complete list of directories used as include directories in a
   * standard compilation process.
   * 
   * @param p the properties object containing some of the include directories
   * @return a non-{@code null} list of include directories
   */
  public static List<File> getIncludeDirectories(Properties p) {
    List<File> includes = new ArrayList<File>();
    includes.addAll(FileUtils.getDirectoriesRecursive(p.targetLib(), ErlConstants.HRL_SUFFIX));
    includes.add(p.include());
    includes.add(p.targetInclude());
    includes.add(p.src());
    return includes;
  }

  /**
   * Gathers a complete list of directories used as include directories in a
   * test compilation process.
   * 
   * @param p the properties object containing some of the include directories
   * @return a non-{@code null} list of include directories
   */
  public static List<File> getTestIncludeDirectories(Properties p) {
    List<File> includes = new ArrayList<File>();
    includes.addAll(FileUtils.getDirectoriesRecursive(p.targetLib(), ErlConstants.HRL_SUFFIX));
    includes.add(p.include());
    includes.add(p.test_src());
    includes.add(p.test_include());
    includes.add(p.targetInclude());
    includes.add(p.src());
    return includes;
  }

  private static List<File> getDependencyCodePaths(Properties p) {
    return FileUtils.getDirectoriesRecursive(p.targetLib(), ErlConstants.BEAM_SUFFIX);
  }

  /**
   * Gathers a complete list of directories used as code paths in an
   * application's build process.
   * 
   * @param p the properties object containing the code path directories
   * @return a non-{@code null} list of code path directories
   */
  public static List<File> getApplicationCodePaths(Properties p) {
    List<File> codePaths = new ArrayList<File>(getDependencyCodePaths(p));
    codePaths.add(p.targetEbin());
    return codePaths;
  }

  /**
   * Gathers a complete list of directories used as code paths in an
   * application's test build process.
   * 
   * @param p the properties object containing the code path directories
   * @return a non-{@code null} list of code path directories
   */
  public static List<File> getApplicationTestCodePaths(Properties p) {
    List<File> codePaths = new ArrayList<File>(getDependencyCodePaths(p));
    codePaths.add(p.targetTestEbin());
    return codePaths;
  }

  /**
   * Gathers a complete list of directories used as code paths in a release
   * build process.
   * 
   * @param p the properties object containing the code path directories
   * @return a non-{@code null} list of code path directories
   */
  public static List<File> getReleaseCodePaths(Properties p) {
    List<File> codePaths = new ArrayList<File>(getDependencyCodePaths(p));
    codePaths.add(p.target());
    return codePaths;
  }
}
