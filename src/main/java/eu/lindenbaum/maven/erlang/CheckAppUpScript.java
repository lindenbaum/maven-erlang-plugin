package eu.lindenbaum.maven.erlang;

import java.io.File;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;

/**
 * A {@link Script} that checks the integrity (correct term format) of an
 * application upgrade file.
 * 
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 */
public class CheckAppUpScript implements Script<String> {
  private static final String script = //
  NL + "lists:flatten(" + NL + //
      "  case file:consult(\"%s\") of" + NL + //
      "      {ok, [{\"%s\", Up, Down}]} when is_list(Up) andalso is_list(Down) ->" + NL + //
      "          lists:foldl(" + NL + //
      "            fun({V, Is}, []) when is_list(Is) ->" + NL + //
      "                    lists:foldl(" + NL + //
      "                      fun(E, []) when is_tuple(E) -> [];" + NL + //
      "                         (E, []) ->" + NL + //
      "                              io_lib:format(\"malformed instruction in ~p: ~p\", [V, E]);" + NL + //
      "                         (_, Acc) -> Acc" + NL + //
      "                      end,  [], Is);" + NL + //
      "               (T, []) -> io_lib:format(\"malformed entry ~p\", [T]);" + NL + //
      "               (_, Acc) -> Acc" + NL + //
      "            end, [], Up ++ Down);" + NL + //
      "      {ok, [{V, _, _}]} ->" + NL + //
      "          io_lib:format(\".appup has invalid version ~p\", [V]);" + NL + //
      "      {ok, _} ->" + NL + //
      "          \".appup file is malformed\";" + NL + //
      "      {error, E = {_, _, _}} ->" + NL + //
      "          file:format_error(E);" + NL + //
      "      {error, Reason} ->" + NL + //
      "          io_lib:format(\"file:consult/1 failed with ~p\", [Reason])" + NL + //
      "  end)." + NL;

  private final File appUpFile;
  private final String version;

  /**
   * Creates a {@link Script} that checks a specific application upgrade file.
   * 
   * @param appUpFile the upgrade file to check
   * @param version the version of the application
   */
  public CheckAppUpScript(File appUpFile, String version) {
    this.appUpFile = appUpFile;
    this.version = version;
  }

  @Override
  public String get() {
    return String.format(script, this.appUpFile.getAbsolutePath(), this.version);
  }

  /**
   * Converts the result of the {@link Script} execution into an error string.
   * 
   * @param result to convert
   * @return An error string describing the error or {@code null} if none.
   */
  @Override
  public String handle(OtpErlangObject result) {
    if (result instanceof OtpErlangString) {
      return ((OtpErlangString) result).stringValue();
    }
    return null;
  }
}
