package eu.lindenbaum.maven.erlang;

import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import eu.lindenbaum.maven.util.ErlUtils;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

import org.apache.maven.plugin.MojoExecutionException;

/**
 * A {@link Script} that can be used to compile erlang files. In case of
 * compiler errors the script will leave the not yet compiled files uncompiled.
 * 
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 */
public final class BeamCompilerScript extends AbstractScript<CompilerResult> {
  private final List<File> files;
  private final List<File> firstFiles;
  private final File outdir;
  private final List<File> includes;
  private final List<String> options;

  /**
   * Creates a compiler script for a {@link List} of erlang files.
   * 
   * @param files a list of files to compile
   * @param firstFiles a list of files to compile before compiling other files
   * @param outdir the destination directory for the compiled .beam
   * @param includes a list of include directories
   * @param options a list of compiler options according to the erlang docs
   * @see <a
   *      href="http://www.erlang.org/doc/man/compile.html">http://www.erlang.org/doc/man/compile.html</a>
   */
  public BeamCompilerScript(List<File> files,
                            List<File> firstFiles,
                            File outdir,
                            List<File> includes,
                            List<String> options) throws MojoExecutionException {
    super();
    this.files = files;
    this.firstFiles = firstFiles;
    this.outdir = outdir;
    this.includes = includes;
    this.options = options;
  }

  @Override
  public String get() {
    String out = this.outdir.getAbsolutePath();
    String incs = ErlUtils.toFilenameList(this.includes, "{i, \"", "\"}");
    String opts = ErlUtils.toList(this.options, null, "", "");
    String files = ErlUtils.toFilenameList(this.files, "\"", "\"");
    String firstFiles = ErlUtils.toFilenameList(this.firstFiles, "\"", "\"");
    return String.format(this.script, out, incs, opts, files, firstFiles);
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

    OtpErlangList f = (OtpErlangList) r.elementAt(0);
    final ArrayList<File> failed = new ArrayList<File>();
    Iterator<OtpErlangObject> failedIterator = f.iterator();
    while (failedIterator.hasNext()) {
      failed.add(new File(ErlUtils.toString(failedIterator.next(), false)));
    }

    OtpErlangList c = (OtpErlangList) r.elementAt(1);
    final ArrayList<File> compiled = new ArrayList<File>();
    Iterator<OtpErlangObject> compiledIterator = c.iterator();
    while (compiledIterator.hasNext()) {
      compiled.add(new File(ErlUtils.toString(compiledIterator.next(), false)));
    }
    Collections.reverse(compiled);

    OtpErlangList e = (OtpErlangList) r.elementAt(2);
    final ArrayList<String> errors = new ArrayList<String>();
    Iterator<OtpErlangObject> errorIterator = e.iterator();
    while (errorIterator.hasNext()) {
      errors.add(ErlUtils.toString(errorIterator.next(), false));
    }

    OtpErlangList w = (OtpErlangList) r.elementAt(3);
    final ArrayList<String> warnings = new ArrayList<String>();
    Iterator<OtpErlangObject> warningIterator = w.iterator();
    while (warningIterator.hasNext()) {
      warnings.add(ErlUtils.toString(warningIterator.next(), false));
    }

    return new CompilerResult() {
      @Override
      public List<File> getFailed() {
        return failed;
      }

      @Override
      public List<File> getCompiled() {
        return compiled;
      }

      @Override
      public List<String> getErrors() {
        return errors;
      }

      @Override
      public List<String> getWarnings() {
        return warnings;
      }
    };
  }
}
