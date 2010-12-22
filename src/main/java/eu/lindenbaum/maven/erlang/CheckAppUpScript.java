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
  "    lists:flatten(" + //
      "  case file:consult(\"%s\") of" + //
      "      {ok, [{\"%s\", Up, Down}]} when is_list(Up) andalso is_list(Down) ->" + //
      "          lists:foldl(" + //
      "            fun({V, Is}, []) when is_list(Is) ->" + //
      "                    lists:foldl(" + //
      "                      fun(E, []) when is_tuple(E) -> [];" + //
      "                         (E, []) ->" + //
      "                              io_lib:format(\"malformed instruction in ~p: ~p\", [V, E]);" + //
      "                         (_, Acc) -> Acc" + //
      "                      end,  [], Is);" + //
      "               (T, []) -> io_lib:format(\"malformed entry ~p\", [T]);" + //
      "               (_, Acc) -> Acc" + //
      "            end, [], Up ++ Down);" + //
      "      {ok, [{V, _, _}]} ->" + //
      "          io_lib:format(\".appup has invalid version ~p\", [V]);" + //
      "      {ok, _} ->" + //
      "          \".appup file is malformed\";" + //
      "      {error, E = {_, _, _}} ->" + //
      "          file:format_error(E);" + //
      "      {error, Reason} ->" + //
      "          io_lib:format(\"file:consult/1 failed with ~p\", [Reason])" + //
      "  end).";

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
