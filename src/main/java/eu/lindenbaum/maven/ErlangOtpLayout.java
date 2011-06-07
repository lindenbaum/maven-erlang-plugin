package eu.lindenbaum.maven;

import java.io.File;
import java.util.Arrays;
import java.util.Collection;

import eu.lindenbaum.maven.util.ErlConstants;

import org.apache.maven.project.MavenProject;

/**
 * Implementation of the {@link SourceLayout} interface for projects with
 * packaging type {@link PackagingType#ERLANG_OTP}. Packaging type for
 * erlang/OTP applications with maven standard directory layout:
 * 
 * <pre>
 *  BASE
 *    +-- src/main/erlang ([ARTIFACTID].app, [ARTIFACTID].appup, *.erl, private *.hrl)
 *    +-- src/main/include (*.hrl)
 *    +-- src/main/priv (*)
 *    +-- src/test/include (*.hrl)
 *    +-- src/test/erlang (*.erl)
 *    +-- src/test/priv (*)
 *    +-- src/site/overview.edoc
 *    +-- target (build artifacts)
 *    +-- pom.xml
 * </pre>
 * 
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 * @since 2.1.0
 */
public class ErlangOtpLayout implements SourceLayout {
  private final MavenProject project;
  private final File base;

  public ErlangOtpLayout(MavenProject project) {
    this.project = project;
    this.base = project.getBasedir();
  }

  @Override
  public File base() {
    return this.base;
  }

  @Override
  public File ebin() {
    return new File(this.base, "src/main/erlang");
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
  public File include() {
    return new File(this.base, "src/main/include");
  }

  @Override
  public File priv() {
    return new File(this.base, "src/main/priv");
  }

  @Override
  public File src() {
    return new File(this.base, "src/main/erlang");
  }

  @Override
  public File testInclude() {
    return new File(this.base, "src/test/include");
  }

  @Override
  public File testPriv() {
    return new File(this.base, "src/test/priv");
  }

  @Override
  public Collection<File> testSrcs() {
    return Arrays.asList(src(), new File(this.base, "src/test/erlang"));
  }

  @Override
  public File overviewEdoc() {
    return new File(new File(new File(this.base, "src"), "site"), ErlConstants.OVERVIEW_EDOC);
  }

  @Override
  public File relFile() {
    return null;
  }

  @Override
  public File relupFile() {
    return null;
  }

  @Override
  public File sysConfigFile() {
    return null;
  }
}
