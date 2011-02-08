package eu.lindenbaum.maven.erlang;

import java.io.File;

import com.ericsson.otp.erlang.OtpErlangObject;

import eu.lindenbaum.maven.util.ErlUtils;

/**
 * A {@link Script} generating <code>edoc</code> documentation for an
 * application using <code>edoc:application/3</code>.
 * 
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 */
public class EDocScript implements Script<Boolean> {
  private static final String script = //
  NL + "In = \"%s\"," + NL + //
      "Out = [{dir, \"%s\"}]," + NL + //
      "Overview = [{overview, \"%s\"}]," + NL + //
      "Options = [{todo, true}, {new, true}, {subpackages, true}]," + NL + //
      "edoc:application('%s', In, Out ++ Overview ++ Options)." + NL;

  private final String appName;
  private final File indir;
  private final File outdir;
  private final File overview;

  /**
   * Creates a {@link Script} that generates <code>edoc</code> documentation for
   * a specfic application.
   * 
   * @param appName name of the application
   * @param indir input directory that will be scanned recursively (where .erl
   *          files reside)
   * @param outdir to put the documentation into
   * @param overview location of the overview file
   */
  public EDocScript(String appName, File indir, File outdir, File overview) {
    this.appName = appName;
    this.indir = indir;
    this.outdir = outdir;
    this.overview = overview;
  }

  @Override
  public String get() {
    String inPath = this.indir.getAbsolutePath();
    String outPath = this.outdir.getAbsolutePath();
    String overviewPath = this.overview.getAbsolutePath();
    return String.format(script, inPath, outPath, overviewPath, this.appName);
  }

  /**
   * Converts the result of the {@link Script} execution into a {@link Boolean}
   * indicating success.
   * 
   * @param result the result term of the {@link Script} execution
   * @return {@code true} on success, {@code false} otherwise
   */
  @Override
  public Boolean handle(OtpErlangObject result) {
    return "ok".equals(ErlUtils.toString(result));
  }
}
