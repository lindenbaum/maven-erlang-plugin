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
 * A {@link Script} that uploads a list of compiled modules and application
 * files onto a specific (remote) node. Modules will be purged ones before
 * loaded, application files will not be written but loaded directly using
 * <code>application:load/1</code>.
 * 
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 */
public class UploadScript extends AbstractScript<GenericScriptResult> {
  private final String remoteNode;
  private final List<File> beamFiles;
  private final List<File> appFiles;
  private final List<File> resourceFiles;

  public UploadScript(String remoteNode, List<File> beamFiles, List<File> appFiles, List<File> resourceFiles) throws MojoExecutionException {
    super();
    this.remoteNode = remoteNode;
    this.beamFiles = beamFiles;
    this.appFiles = appFiles;
    this.resourceFiles = resourceFiles;
  }

  @Override
  public String get() {
    String beamFileList = ErlUtils.toFilenameList(this.beamFiles, "\"", "\"");
    String appFileList = ErlUtils.toFilenameList(this.appFiles, "\"", "\"");
    String resourceList = ErlUtils.toFilenameList(this.resourceFiles, "\"", "\"");
    return String.format(this.script, this.remoteNode, beamFileList, appFileList, resourceList);
  }

  /**
   * Converts the result of the {@link Script} execution into a generic result.
   * 
   * @return an instance of {@link GenericScriptResult}
   */
  @Override
  public GenericScriptResult handle(final OtpErlangObject result) {
    OtpErlangTuple resultTuple = (OtpErlangTuple) result;
    OtpErlangList succeeded = (OtpErlangList) resultTuple.elementAt(0);
    OtpErlangList failed = (OtpErlangList) resultTuple.elementAt(1);

    final boolean success = failed.arity() == 0;
    final String node = this.remoteNode;

    final List<File> successFullyUploaded = new ArrayList<File>();
    Iterator<OtpErlangObject> succeededIterator = succeeded.iterator();
    while (succeededIterator.hasNext()) {
      successFullyUploaded.add(new File(ErlUtils.toString(succeededIterator.next())));
    }

    final List<File> failedToUpload = new ArrayList<File>();
    Iterator<OtpErlangObject> failedIterator = failed.iterator();
    while (failedIterator.hasNext()) {
      failedToUpload.add(new File(ErlUtils.toString(failedIterator.next())));
    }

    return new GenericScriptResult() {
      @Override
      public boolean success() {
        return success;
      }

      @Override
      public void logOutput(Log log) {
        if (!successFullyUploaded.isEmpty()) {
          log.info("Files uploaded successfully to " + node + ":");
          MavenUtils.logCollection(log, LogLevel.INFO, successFullyUploaded, " * ");
          log.info("");
        }
        if (!failedToUpload.isEmpty()) {
          log.error("Files that could not be uploaded to " + node + ":");
          MavenUtils.logCollection(log, LogLevel.ERROR, failedToUpload, " * ");
        }
      }
    };
  }
}
