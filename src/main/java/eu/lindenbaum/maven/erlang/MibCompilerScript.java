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
 * A {@link Script} that can be used to compile MIB files.
 * 
 * @author Olle Törnström <olle.toernstroem@lindenbaum.eu>
 * @since 2.2.0
 */
public final class MibCompilerScript extends AbstractScript<CompilerResult> {
  private final List<File> files;
  private final File outdir;
  private final List<File> includes;

  /**
   * Creates a compiler script for a {@link List} of MIB files.
   * 
   * @param files a list of the MIB files to compile
   * @param outdir the destination directory for the compiled .bin MIBs
   * @param includes a list of include directories
   */
  public MibCompilerScript(List<File> files, File outdir, List<File> includes) throws MojoExecutionException {
    super();
    this.files = files;
    this.outdir = outdir;
    this.includes = includes;
  }

  @Override
  public String get() {
    String out = this.outdir.getAbsolutePath();
    String incs = ErlUtils.toFilenameList(this.includes, "\"", "\"");
    String files = ErlUtils.toFilenameList(this.files, "\"", "\"");
    return String.format(this.script, out, incs, files);
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
        return Collections.emptyList();
      }
    };
  }
}
