package eu.lindenbaum.maven.erlang;

import java.io.File;

import eu.lindenbaum.maven.util.ErlConstants;
import eu.lindenbaum.maven.util.ErlUtils;
import eu.lindenbaum.maven.util.MavenUtils;
import eu.lindenbaum.maven.util.MavenUtils.LogLevel;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

import org.apache.maven.plugin.logging.Log;

/**
 * A {@link Script} generating release scripts using
 * <code>systools:make_script/2</code>.
 * 
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 * @author Olle Törnström <olle.toernstroem@lindenbaum.eu>
 */
public final class MakeScriptScript implements Script<GenericScriptResult> {
  private static final String script = //
  NL + "case systools:make_script(\"%s\", [silent, {outdir, \"%s\"}] ++ [%s]) of" + NL + //
      "    ok -> {ok, \"\"};" + NL + //
      "    error -> {error, \"unknown\"};" + NL + //
      "    {ok, Module, Warnings} ->" + NL + //
      "        {warn, lists:flatten(Module:format_warning(Warnings))};" + NL + //
      "    {error, Module, Error} ->" + NL + //
      "        {error, lists:flatten(Module:format_error(Error))}" + NL + //
      "end." + NL;

  private final File releaseFile;
  private final File outdir;
  private final String options;

  /**
   * Creates a {@link Script} generating the erlang boot scripts for an OTP
   * release.
   * 
   * @param releaseFile to generate scripts for
   * @param outdir to place the generated scripts into
   * @param options additional info according to erlang documentation, maybe
   *          {@code null}
   * @see <a
   *      href="http://www.erlang.org/doc/man/systools.html">http://www.erlang.org/doc/man/systools.html</a>
   */
  public MakeScriptScript(File releaseFile, File outdir, String options) {
    this.releaseFile = releaseFile;
    this.outdir = outdir;
    this.options = options != null ? options : "";
  }

  @Override
  public String get() {
    String rel = this.releaseFile.getAbsolutePath().replace(ErlConstants.REL_SUFFIX, "");
    String outPath = this.outdir.getAbsolutePath();
    return String.format(script, rel, outPath, this.options);
  }

  /**
   * Converts the result of the {@link Script} execution into an object capable
   * of logging the errors/warnings as well as returning the script result.
   * 
   * @param result The return term of the {@link Script} execution.
   * @return An object capable of delivering the results transparently.
   */
  @Override
  public GenericScriptResult handle(OtpErlangObject result) {
    OtpErlangTuple resultTuple = (OtpErlangTuple) result;
    final String level = ErlUtils.toString(resultTuple.elementAt(0));
    final String messages = ErlUtils.toString(resultTuple.elementAt(1));
    return new GenericScriptResult() {
      @Override
      public boolean success() {
        return "ok".equals(level) || "warn".equals(level);
      }

      @Override
      public void logOutput(Log log) {
        if (!messages.isEmpty()) {
          LogLevel logLevel = LogLevel.fromString(level);
          if (logLevel != LogLevel.INFO) {
            MavenUtils.logMultiLineString(log, logLevel, messages);
          }
        }
      }
    };
  }
}
