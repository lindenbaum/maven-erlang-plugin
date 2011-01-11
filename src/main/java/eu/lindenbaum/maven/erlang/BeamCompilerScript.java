package eu.lindenbaum.maven.erlang;

import java.io.File;
import java.util.List;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

import eu.lindenbaum.maven.util.ErlUtils;

import org.apache.maven.plugin.logging.Log;

/**
 * A {@link Script} that can be used to compile erlang files. In case of
 * compiler errors the script will leave the not yet compiled files uncompiled.
 * 
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 */
public final class BeamCompilerScript implements Script<CompilerResult> {
  private static final String script = //
  NL + "Options = [return, {outdir, \"%s\"}] ++ %s ++ %s," + NL + //
      "lists:foldl(" + NL + //
      "  fun(ToCompile, {\"\", Reports}) ->" + NL + //
      "          {Fail, Messages} = case compile:file(ToCompile, Options) of" + NL + //
      "                               {error, E, W} ->" + NL + //
      "                                   {ToCompile," + NL + //
      "                                    lists:map(fun(Elem) -> {error, Elem} end, E)" + NL + //
      "                                    ++ lists:map(fun(Elem) -> {warn, Elem} end, W)};" + NL + //
      "                               {ok, _, W} ->" + NL + //
      "                                   {\"\"," + NL + //
      "                                    lists:map(fun(Elem) -> {warn, Elem} end, W)}" + NL + //
      "                             end," + NL + //
      "          R = lists:foldr(" + NL + //
      "                fun({Level, {File, Exceptions}}, Acc) ->" + NL + //
      "                        lists:foldr(fun({Line, Module, Info}, A) ->" + NL + //
      "                                            Formatted = Module:format_error(Info)," + NL + //
      "                                            Flattened = lists:flatten(Formatted)," + NL + //
      "                                            F = io_lib:format(\"~s:~p:\", [File, Line])," + NL + //
      "                                            S = io_lib:format(\"~s\", [Flattened])," + NL + //
      "                                            [{Level, lists:flatten(F)}," + NL + //
      "                                             {Level, lists:flatten(S)}] ++ A;" + NL + //
      "                                       (Else, A) ->" + NL + //
      "                                            F = io_lib:format(\"~s:\", [File])," + NL + //
      "                                            S = io_lib:format(\"~p\", [Else])," + NL + //
      "                                            [{Level, lists:flatten(F)}," + NL + //
      "                                             {Level, lists:flatten(S)}] ++ A" + NL + //
      "                                    end, Acc, Exceptions)" + NL + //
      "                end, [], Messages)," + NL + //
      "          {Fail, Reports ++ R};" + NL + //
      "     (_, Result) ->" + NL + //
      "          Result" + NL + //
      "  end, {\"\", []}, %s)." + NL;

  private final List<File> files;
  private final File outdir;
  private final List<File> includes;
  private final List<String> options;

  /**
   * Creates a compiler script for a {@link List} of erlang files.
   * 
   * @param files a list of files to compile
   * @param outdir the destination directory for the compiled .beam
   * @param includes a list of include directories
   * @param options a list of compiler options according to the erlang docs
   * @see http://www.erlang.org/doc/man/compile.html
   */
  public BeamCompilerScript(List<File> files, File outdir, List<File> includes, List<String> options) {
    this.files = files;
    this.outdir = outdir;
    this.includes = includes;
    this.options = options;
  }

  @Override
  public String get() {
    String out = this.outdir.getAbsolutePath();
    String incs = ErlUtils.toFileList(this.includes, "{i, \"", "\"}");
    String opts = ErlUtils.toList(this.options, null, "", "");
    String files = ErlUtils.toFileList(this.files, "\"", "\"");
    return String.format(script, out, incs, opts, files);
  }

  /**
   * Converts the result of the {@link Script} execution into an object logging
   * the compiler output correctly as well as providing failed compilation
   * units, if any.
   * 
   * @param result The return term of the {@link Script} execution.
   * @return An object capable of delivering the results transparently.
   */
  @Override
  public CompilerResult handle(OtpErlangObject result) {
    OtpErlangTuple r = (OtpErlangTuple) result;
    final OtpErlangObject failed = r.elementAt(0);
    final OtpErlangList messages = (OtpErlangList) r.elementAt(1);
    return new CompilerResult() {
      @Override
      public void logOutput(Log log) {
        for (int i = 0; i < messages.arity(); ++i) {
          OtpErlangTuple messageTuple = (OtpErlangTuple) messages.elementAt(i);
          OtpErlangAtom level = (OtpErlangAtom) messageTuple.elementAt(0);
          if ("error".equals(level.atomValue())) {
            log.error(ErlUtils.cast(messageTuple.elementAt(1)));
          }
          else {
            log.warn(ErlUtils.cast(messageTuple.elementAt(1)));
          }
        }
      }

      @Override
      public String getFailed() {
        if (failed instanceof OtpErlangString) {
          String converted = ((OtpErlangString) failed).stringValue().trim();
          return converted.isEmpty() ? null : converted;
        }
        return null;
      }
    };
  }
}
