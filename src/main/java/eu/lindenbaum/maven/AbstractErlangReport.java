package eu.lindenbaum.maven;

import static eu.lindenbaum.maven.util.ErlUtils.eval;
import static eu.lindenbaum.maven.util.FileUtils.SOURCE_FILTER;

import java.io.File;

import org.apache.maven.doxia.siterenderer.Renderer;
import org.apache.maven.plugin.Mojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.project.MavenProject;
import org.apache.maven.reporting.AbstractMavenReport;

/**
 * A base class for report generating {@link Mojo}s.
 * 
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 */
abstract class AbstractErlangReport extends AbstractMavenReport {
  /**
   * {@link MavenProject} to process.
   * 
   * @parameter expression="${project}"
   * @required
   * @readonly
   */
  private MavenProject project;

  /**
   * Doxia Site Renderer.
   * 
   * @parameter expression="${component.org.codehaus.doxia.site.renderer.DefaultSiteRenderer}"
   * @required
   * @readonly
   */
  private Renderer renderer;

  @Override
  protected MavenProject getProject() {
    return this.project;
  }

  @Override
  protected Renderer getSiteRenderer() {
    return this.renderer;
  }

  @Override
  public boolean isExternalReport() {
    return true;
  }

  /**
   * Generate the documentation with edoc for a set of files.
   * 
   * @param srcDir directory where sources reside
   * @param outDir directory to put edoc in
   * @param options optional edoc parameters, maybe {@code null}
   * @throws MojoExecutionException
   */
  protected void generateEDoc(File srcDir, File outDir, String[] options) throws MojoExecutionException {
    StringBuilder eDocExpr = new StringBuilder();
    eDocExpr.append("edoc:files([");
    File[] sources = srcDir.listFiles(SOURCE_FILTER);
    for (int i = 0; i < sources.length; ++i) {
      if (i != 0) {
        eDocExpr.append(", ");
      }
      eDocExpr.append("\"");
      eDocExpr.append(sources[i].getPath());
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
   * @throws MojoExecutionException
   */
  protected void generateAppEDoc(String app, File srcDir, File outDir, String[] options) throws MojoExecutionException {
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
