package eu.lindenbaum.maven;

import java.io.File;

import eu.lindenbaum.maven.util.ErlConstants;

import org.apache.maven.project.MavenProject;

/**
 * Implementation of the {@link TargetLayout} bean defining the project's build
 * directory layout.
 * 
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 * @since 2.1.0
 */
public class DefaultTargetLayout implements TargetLayout {
  private final MavenProject project;
  private final File base;

  /**
   * <p>
   * Defines the layout of the target directory relevant for maven-erlang
   * projects. Standard layout is:
   * 
   * <pre>
   * target
   *   +-- [ARTIFACTID]-[Version]
   *   +-- [ARTIFACTID]-[Version]/ebin (*.beam, [ARTIFACTID].app, [ARTIFACTID].appup)
   *   +-- [ARTIFACTID]-[Version]/include (*.hrl)
   *   +-- [ARTIFACTID]-[Version]/priv (*)
   *   +-- [ARTIFACTID]-[Version]/src (*.erl)
   *   +-- [ARTIFACTID]-[Version]-test
   *   +-- [ARTIFACTID]-[Version]-test/ebin (*.beam)
   *   +-- [ARTIFACTID]-[Version]-test/priv (*)
   *   +-- surefire-reports (*.html)
   *   +-- profiling-reports (*.html)
   *   +-- relup
   *   +-- sys.config
   *   +-- [ARTIFACTID].rel
   *   +-- .dialyzer.ok
   * </pre>
   * </p>
   * 
   * @param project The project to build the target layout for.
   */
  public DefaultTargetLayout(MavenProject project) {
    this.project = project;
    this.base = new File(project.getBuild().getDirectory());
  }

  @Override
  public File base() {
    return this.base;
  }

  @Override
  public File lib() {
    return new File(this.base, "lib");
  }

  @Override
  public File projectArtifact() {
    String name = this.project.getArtifactId() + "-" + this.project.getVersion();
    return new File(this.base, name + ErlConstants.TARGZ_SUFFIX);
  }

  @Override
  public File dialyzerOk() {
    return new File(this.base, ErlConstants.DIALYZER_OK);
  }

  // applications (erlang-std/erlang-otp) *************************************/

  @Override
  public File project() {
    return new File(this.base, this.project.getArtifactId() + "-" + this.project.getVersion());
  }

  @Override
  public File appFile() {
    return new File(ebin(), this.project.getArtifactId() + ErlConstants.APP_SUFFIX);
  }

  @Override
  public File appupFile() {
    return new File(ebin(), this.project.getArtifactId() + ErlConstants.APPUP_SUFFIX);
  }

  @Override
  public File ebin() {
    return new File(project(), "ebin");
  }

  @Override
  public File include() {
    return new File(project(), "include");
  }

  @Override
  public File priv() {
    return new File(project(), "priv");
  }

  @Override
  public File src() {
    return new File(project(), "src");
  }

  @Override
  public File test() {
    return new File(this.base, this.project.getArtifactId() + "-" + this.project.getVersion() + "-test");
  }

  @Override
  public File testEbin() {
    return new File(test(), "ebin");
  }

  @Override
  public File testPriv() {
    return new File(test(), "priv");
  }

  @Override
  public File overviewEdoc() {
    return new File(this.base, ErlConstants.OVERVIEW_EDOC);
  }

  @Override
  public File surefireReports() {
    return new File(this.base, "surefire-reports");
  }

  @Override
  public File profilingReports() {
    return new File(this.base, "profiling-reports");
  }

  // release (erlang-rel) *****************************************************/

  @Override
  public File relFile() {
    String name = this.project.getArtifactId() + "-" + this.project.getVersion();
    return new File(this.base, name + ErlConstants.REL_SUFFIX);
  }

  @Override
  public File relupFile() {
    return new File(this.base, ErlConstants.RELUP);
  }

  @Override
  public File sysConfigFile() {
    return new File(this.base, ErlConstants.SYS_CONFIG);
  }
}
