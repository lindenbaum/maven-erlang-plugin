package eu.lindenbaum.maven;

import static eu.lindenbaum.maven.util.ErlConstants.ERL_SUFFIX;
import static eu.lindenbaum.maven.util.ErlUtils.eval;
import static eu.lindenbaum.maven.util.FileUtils.getFilesRecursive;

import java.io.File;
import java.util.List;

import org.apache.maven.plugin.Mojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;

/**
 * A base class for {@link Mojo}s generating EDoc reports.
 * 
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 */
abstract class AbstractEDocReport extends AbstractErlangReport {
  /**
   * Generate the documentation with edoc for a set of files.
   * 
   * @param srcDir directory where sources reside
   * @param outDir directory to put edoc in
   * @param options optional edoc parameters, maybe {@code null}
   * @throws MojoFailureException
   * @throws MojoExecutionException
   */
  protected void generateEDoc(File srcDir, File outDir, String[] options) throws MojoExecutionException,
                                                                         MojoFailureException {
    StringBuilder eDocExpr = new StringBuilder();
    eDocExpr.append("edoc:files([");
    List<File> sources = getFilesRecursive(srcDir, ERL_SUFFIX);
    for (int i = 0; i < sources.size(); ++i) {
      if (i != 0) {
        eDocExpr.append(", ");
      }
      eDocExpr.append("\"");
      eDocExpr.append(sources.get(i).getPath());
      eDocExpr.append("\"");
    }
    eDocExpr.append("],");
    eDocExpr.append("[{dir, \"");
    eDocExpr.append(outDir.getPath());
    eDocExpr.append("\"},");
    eDocExpr.append(" {source_path, [\"");
    eDocExpr.append(srcDir.getPath());
    eDocExpr.append("\"]}");
    if (options != null) {
      for (String option : options) {
        eDocExpr.append(",");
        eDocExpr.append(option);
      }
    }
    eDocExpr.append("]).");
    eval(getLog(), eDocExpr.toString());
  }

  /**
   * Generate the documentation with edoc for an OTP application.
   * 
   * @param app application name
   * @param srcDir directory where sources and .app file reside
   * @param outDir directory to put edoc in
   * @param options optional edoc parameters, maybe {@code null}
   * @throws MojoFailureException
   * @throws MojoExecutionException
   */
  protected void generateAppEDoc(String app, File srcDir, File outDir, String[] options) throws MojoExecutionException,
                                                                                        MojoFailureException {
    StringBuilder eDocExpr = new StringBuilder();
    eDocExpr.append("edoc:application(\'");
    eDocExpr.append(app);
    eDocExpr.append("\',\"");
    eDocExpr.append(srcDir.getPath());
    eDocExpr.append("\",");
    eDocExpr.append("[{dir, \"");
    eDocExpr.append(outDir.getPath());
    eDocExpr.append("\"},");
    eDocExpr.append(" {source_path, [\"");
    eDocExpr.append(srcDir.getPath());
    eDocExpr.append("\"]}");
    if (options != null) {
      for (String option : options) {
        eDocExpr.append(",");
        eDocExpr.append(option);
      }
    }
    eDocExpr.append("]).");
    eval(getLog(), eDocExpr.toString());
  }
}
