package eu.lindenbaum.maven.erlang;

import java.io.File;
import java.util.List;

import eu.lindenbaum.maven.util.ErlUtils;
import eu.lindenbaum.maven.util.MavenUtils;
import eu.lindenbaum.maven.util.MavenUtils.LogLevel;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;

import org.apache.maven.plugin.logging.Log;

/**
 * A {@link Script} that uploads a list of compiled modules and application
 * files onto a specific (remote) node. Modules will be purged ones before
 * loaded, application files will not be written but loaded directly using
 * <code>application:load/1</code>.
 * 
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 */
public class UploadScript implements Script<GenericScriptResult> {
  private static String script = //
  NL + "Node = '%s'," + NL + //
      "BeamFileList = %s," + NL + //
      "ApplicationFileList = %s," + NL + //
      "case net_kernel:connect(Node) of" + NL + //
      "    true ->" + NL + //
      "        E = lists:foldl(" + NL + //
      "              fun(BeamFile, Ok) when is_list(Ok) ->" + NL + //
      "                      Module = filename:basename(BeamFile, \".beam\")," + NL + //
      "                      case file:read_file(BeamFile) of" + NL + //
      "                          {ok, Binary} ->" + NL + //
      "                              rpc:eval_everywhere(" + NL + //
      "                                [Node], code, purge," + NL + //
      "                                [list_to_atom(Module)])," + NL + //
      "                              rpc:eval_everywhere(" + NL + //
      "                                [Node], code, load_binary," + NL + //
      "                                [list_to_atom(Module)," + NL + //
      "                                 BeamFile, Binary])," + NL + //
      "                              [BeamFile | Ok];" + NL + //
      "                          Other ->" + NL + //
      "                              {BeamFile, Other}" + NL + //
      "                      end;" + NL + //
      "                 (_, Error) ->" + NL + //
      "                      Error" + NL + //
      "              end, [], BeamFileList)," + NL + //
      "        lists:foldl(" + NL + //
      "          fun(AppFile, Ok) when is_list(Ok) ->" + NL + //
      "                  case file:consult(AppFile) of" + NL + //
      "                      {ok, [AppSpec]} ->" + NL + //
      "                          rpc:eval_everywhere(" + NL + //
      "                            [Node], application," + NL + //
      "                            load, [AppSpec])," + NL + //
      "                            [AppFile | Ok];" + NL + //
      "                      Other ->" + NL + //
      "                          {AppFile, Other}" + NL + //
      "                  end;" + NL + //
      "             (_, Error) ->" + NL + //
      "                  Error" + NL + //
      "          end, E, ApplicationFileList);" + NL + //
      "    false ->" + NL + //
      "        {error, {cannot_connect, Node}}" + NL + //
      " end.";

  private final String remoteNode;
  private final List<File> beamFiles;
  private final List<File> appFiles;

  public UploadScript(String remoteNode, List<File> beamFiles, List<File> appFiles) {
    this.remoteNode = remoteNode;
    this.beamFiles = beamFiles;
    this.appFiles = appFiles;
  }

  @Override
  public String get() {
    String beamFileList = ErlUtils.toFileList(this.beamFiles, "\"", "\"");
    String appFileList = ErlUtils.toFileList(this.appFiles, "\"", "\"");
    return String.format(script, this.remoteNode, beamFileList, appFileList);
  }

  /**
   * Converts the result of the {@link Script} execution into a generic result.
   * 
   * @return an instance of {@link GenericScriptResult}
   */
  @Override
  public GenericScriptResult handle(final OtpErlangObject result) {
    return new GenericScriptResult() {
      @Override
      public boolean success() {
        return result instanceof OtpErlangList;
      }

      @Override
      public void logOutput(Log log) {
        final LogLevel level;
        final String multiLineString;
        if (!success()) {
          level = LogLevel.ERROR;
          multiLineString = ErlUtils.toString(result);
        }
        else {
          level = LogLevel.INFO;
          multiLineString = ErlUtils.toString(result).replace(",", "," + NL);
        }
        MavenUtils.logMultiLineString(log, level, multiLineString);
      }
    };
  }
}
