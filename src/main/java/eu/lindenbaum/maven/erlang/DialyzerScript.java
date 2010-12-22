package eu.lindenbaum.maven.erlang;

import java.io.File;
import java.util.List;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;

import eu.lindenbaum.maven.util.ErlUtils;

/**
 * A {@link Script} that can be used to dialyze erlang files.
 * 
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 */
public final class DialyzerScript implements Script<String[]> {
  private static final String script = //
  "    Options = [{from, src_code}," + //
      "           {get_warnings, true}," + //
      "           {files_rec, %s}," + //
      "           {include_dirs, %s}," + //
      "           {warnings, %s}]," + //
      "lists:map(fun(Warning) ->" + //
      "              S = dialyzer:format_warning(Warning)," + //
      "              lists:flatten(S)" + //
      "          end, dialyzer:run(Options)).";

  private final List<File> files;
  private final List<File> includes;
  private final String options;

  /**
   * Creates a dialyzer script for a {@link List} of erlang source files,
   * includes.
   * 
   * @param files a list of files to dialyze (from source)
   * @param includes a list of include files
   * @param options a list of dialyzer options according to the erlang docs
   * @see http://www.erlang.org/doc/man/dialyzer.html
   */
  public DialyzerScript(List<File> files, List<File> includes, String options) {
    this.files = files;
    this.includes = includes;
    this.options = options;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String get() {
    String files = ErlUtils.toFileList(this.files, "\"", "\"");
    String incs = ErlUtils.toFileList(this.includes, "\"", "\"");
    String opts = "[" + (this.options != null ? this.options : "") + "]";
    return String.format(script, files, incs, opts);
  }

  /**
   * Converts the result of the {@link Script} execution into a list of dialyzer
   * warning strings.
   * 
   * @param result The return term of the {@link Script} execution.
   * @return A list of warning strings issued by the erlang dialyzer.
   */
  @Override
  public String[] handle(OtpErlangObject result) {
    OtpErlangList warnings = (OtpErlangList) result;
    String[] messages = new String[warnings.arity()];
    for (int i = 0; i < warnings.arity(); ++i) {
      messages[i] = ErlUtils.cast(warnings.elementAt(i));
    }
    return messages;
  }
}
