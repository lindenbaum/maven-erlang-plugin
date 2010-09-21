package eu.lindenbaum.maven.util;

import static eu.lindenbaum.maven.util.ErlUtils.eval;
import static eu.lindenbaum.maven.util.FileUtils.SOURCE_FILTER;

import java.io.File;

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;

/**
 * Containing utilities related to erlang documentation generation.
 * 
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 * @author Timo Koepke <timo.koepke@lindenbaum.eu>
 */
public final class EDocUtils {
  /**
   * Generate the documentation with edoc for a set of files.
   * 
   * @param log logger to use
   * @param srcDir directory where sources reside
   * @param outDir directory to put edoc in
   * @param options optional edoc parameters, maybe {@code null}
   * @throws MojoExecutionException
   */
  public static void generateEDoc(Log log, File srcDir, File outDir, String[] options) throws MojoExecutionException {
    StringBuilder eDocExpr = new StringBuilder();
    eDocExpr.append("edoc:files([");
    File[] sources = srcDir.listFiles(SOURCE_FILTER);
    for (int i = 0; i < sources.length; ++i) {
      if (i != 0) {
        eDocExpr.append(", ");
      }
      eDocExpr.append("\"");
      eDocExpr.append(sources[i].getAbsolutePath());
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
    eval(log, eDocExpr.toString());
  }

  /**
   * Generate the documentation with edoc for an OTP application.
   * 
   * @param log logger to use
   * @param app application name
   * @param srcDir directory where sources and .app file reside
   * @param outDir directory to put edoc in
   * @param options optional edoc parameters, maybe {@code null}
   * @throws MojoExecutionException
   */
  public static void generateAppEDoc(Log log, String app, File srcDir, File outDir, String[] options) throws MojoExecutionException {
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
    eval(log, eDocExpr.toString());
  }
}
