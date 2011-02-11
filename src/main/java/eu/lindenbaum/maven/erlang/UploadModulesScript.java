package eu.lindenbaum.maven.erlang;

import java.io.File;
import java.util.List;

import com.ericsson.otp.erlang.OtpErlangObject;

import eu.lindenbaum.maven.util.ErlUtils;

/**
 * A {@link Script} that uploads a list of compiled modules onto a specific
 * (remote) node.
 * 
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 */
public class UploadModulesScript implements Script<String> {
  private static String script = //
  NL + "Node = '%s'," + NL + //
      "BeamFileList = %s," + NL + //
      "case net_kernel:connect(Node) of" + NL + //
      "    true ->" + NL + //
      "        lists:foldl(" + NL + //
      "          fun(BeamFile, ok) ->" + NL + //
      "                  Module = filename:basename(BeamFile, \".beam\")," + NL + //
      "                  case file:read_file(BeamFile) of" + NL + //
      "                      {ok, Binary} ->" + NL + //
      "                          rpc:eval_everywhere(" + NL + //
      "                            [Node], code, load_binary," + NL + //
      "                            [list_to_atom(Module), BeamFile, Binary])," + NL + //
      "                          ok;" + NL + //
      "                      Other ->" + NL + //
      "                          {BeamFile, Other}" + NL + //
      "                  end;" + NL + //
      "             (_, Error) ->" + NL + //
      "                  Error" + NL + //
      "          end, ok, BeamFileList);" + NL + //
      "    false ->" + NL + //
      "        {error, {cannot_connect, Node}}" + NL + //
      "end.";

  private final String remoteNode;
  private final List<File> beamFiles;

  public UploadModulesScript(String remoteNode, List<File> beamFiles) {
    this.remoteNode = remoteNode;
    this.beamFiles = beamFiles;
  }

  @Override
  public String get() {
    String beamFileList = ErlUtils.toFileList(this.beamFiles, "\"", "\"");
    return String.format(script, this.remoteNode, beamFileList);
  }

  /**
   * Converts the result of the {@link Script} execution into an error message.
   * 
   * @return "ok" in case of success, a failure message in case of errors
   */
  @Override
  public String handle(OtpErlangObject result) {
    return ErlUtils.toString(result);
  }
}
