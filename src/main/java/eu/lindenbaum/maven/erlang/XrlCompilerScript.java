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

public class XrlCompilerScript extends AbstractScript<CompilerResult> {
  private final List<File> xrlFiles;
  private final File outDir;

  public XrlCompilerScript(List<File> xrlFiles, File outDir) throws MojoExecutionException {
    super();
    this.xrlFiles = xrlFiles;
    this.outDir = outDir;
  }

  @Override
  public String get() {
    String out = this.outDir.getAbsolutePath();
    String files = ErlUtils.toFilenameList(this.xrlFiles, "\"", "\"");
    return String.format(this.script, out, files);
  }

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
