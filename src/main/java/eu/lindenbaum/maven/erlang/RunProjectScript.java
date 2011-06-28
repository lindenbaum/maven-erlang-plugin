package eu.lindenbaum.maven.erlang;

import java.io.File;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import eu.lindenbaum.maven.util.ErlUtils;
import eu.lindenbaum.maven.util.MavenUtils;
import eu.lindenbaum.maven.util.MavenUtils.LogLevel;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;

/**
 * A {@link Script} starting a list of erlang applications on a specific node.
 * 
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 * @author Olle Törnström <olle.toernstroem@lindenbaum.eu>
 * @author Timo Koepke <timo.koepke@lindenbaum.eu>
 */
public class RunProjectScript extends AbstractScript<GenericScriptResult> {
  private final String node;
  private final List<String> applications;
  private final File sysConfigFile;

  /**
   * Creates a {@link Script} trying to start a set of erlang applications.
   * 
   * @param node to start the applications on
   * @param applications to start
   */
  public RunProjectScript(String node, List<String> applications) throws MojoExecutionException {
    this(node, applications, new File("."));
  }

  /**
   * Creates a {@link Script} trying to start a set of erlang applications.
   * 
   * @param node to start the applications on
   * @param applications to start
   * @param sysConfigFile a system configuration file to load before starting
   *          the applications
   */
  public RunProjectScript(String node, List<String> applications, File sysConfigFile) throws MojoExecutionException {
    super();
    this.node = node;
    this.applications = applications;
    this.sysConfigFile = sysConfigFile;
  }

  @Override
  public String get() {
    String applications = ErlUtils.toList(this.applications, null, "'", "'");
    String configFile = this.sysConfigFile.getAbsolutePath();
    return String.format(this.script, this.node, applications, configFile);
  }

  /**
   * Converts the result of the {@link Script} execution into an object capable
   * of logging the script output as well as returning whether the script
   * succeeded.
   * 
   * @param result The return term of the {@link Script} execution.
   * @return An object capable of delivering the results transparently.
   */
  @Override
  public GenericScriptResult handle(OtpErlangObject result) {
    OtpErlangTuple resultTuple = (OtpErlangTuple) result;
    OtpErlangList succeeded = (OtpErlangList) resultTuple.elementAt(0);
    OtpErlangList failed = (OtpErlangList) resultTuple.elementAt(1);

    final List<String> succeededApplications = new ArrayList<String>();
    Iterator<OtpErlangObject> succeededIterator = succeeded.iterator();
    while (succeededIterator.hasNext()) {
      succeededApplications.add(ErlUtils.toString(succeededIterator.next()));
    }

    final List<String> failedApplications = new ArrayList<String>();
    Iterator<OtpErlangObject> failedIterator = failed.iterator();
    while (failedIterator.hasNext()) {
      failedApplications.add(ErlUtils.toString(failedIterator.next()));
    }

    return new GenericScriptResult() {
      @Override
      public boolean success() {
        return failedApplications.isEmpty();
      }

      @Override
      public void logOutput(Log log) {
        if (!succeededApplications.isEmpty()) {
          log.info("Applications successfully started:");
          MavenUtils.logCollection(log, LogLevel.INFO, succeededApplications, " * ");
          log.info("");
        }
        if (!failedApplications.isEmpty()) {
          log.error("Applications that could not be started:");
          MavenUtils.logCollection(log, LogLevel.ERROR, failedApplications, " * ");
        }
      }
    };
  }
}
