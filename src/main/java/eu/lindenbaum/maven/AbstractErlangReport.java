package eu.lindenbaum.maven;

import java.io.File;

import org.apache.maven.doxia.siterenderer.Renderer;
import org.apache.maven.plugin.Mojo;
import org.apache.maven.project.MavenProject;
import org.apache.maven.reporting.AbstractMavenReport;

/**
 * A base class for report generating {@link Mojo}s. Its only use is to keep
 * all path definitions in a single place in order to be easily customizable.
 * 
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 * @see AbstractErlangMojo
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
   * @component
   * @required
   * @readonly
   */
  private Renderer renderer;

  /**
   * Directory where the erlang source folder reside. It is also assumed that
   * the application specific application resource file and (if any) the release
   * files are located here.
   * Default is: {@code src/main/erlang}.
   * 
   * @parameter expression="${basedir}/src/main/erlang"
   * @required
   * @readonly
   */
  File srcMainErlang;

  /**
   * Directory where the header files reside.
   * Default is: {@code src/main/include}.
   * 
   * @parameter expression="${basedir}/src/main/include"
   * @required
   * @readonly
   */
  File srcMainInclude;

  /**
   * Directory where the erlang test source files reside.
   * Default is: {@code src/test/erlang}.
   * 
   * @parameter expression="${basedir}/src/test/erlang/"
   * @required
   * @readonly
   */
  File srcTestErlang;

  /**
   * Directory where the erlang test include files reside.
   * Default is: {@code src/test/include}.
   * 
   * @parameter expression="${basedir}/src/test/include"
   * @required
   * @readonly
   */
  File srcTestInclude;

  /**
   * Directory where the compiled test sources and recompiled sources will be
   * placed into. Default is: {@code target/test}.
   * 
   * @parameter expression="${project.build.directory}/test"
   * @required
   * @readonly
   */
  File targetTest;

  /**
   * Directory to generate the EDoc report into.
   * Default is: {@code target/site/doc}.
   * 
   * @parameter expression="${project.reporting.outputDirectory}/doc"
   * @required
   * @readonly
   */
  File targetSiteDoc;

  /**
   * Directory to generate the EDoc test report into.
   * Default is: {@code target/site/tes-doc}.
   * 
   * @parameter expression="${project.reporting.outputDirectory}/test-doc"
   * @required
   * @readonly
   */
  File targetSiteTestDoc;

  /**
   * Directory to generate the coverage report into.
   * Default is: {@code target/site}.
   *
   * @parameter expression="${project.reporting.outputDirectory}"
   * @required
   * @readonly
   */
  File targetSiteCoverage;

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
}
