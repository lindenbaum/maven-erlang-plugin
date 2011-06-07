package eu.lindenbaum.maven;

import java.io.File;
import java.util.Arrays;
import java.util.Collection;

import eu.lindenbaum.maven.util.ErlConstants;

import org.apache.maven.project.MavenProject;

/**
 * Implementation of the {@link SourceLayout} interface for projects with
 * packaging type {@link PackagingType#ERLANG_STD}. Packaging type for
 * erlang/OTP applications with erlang/OTP standard directory layout:
 * 
 * <pre>
 *  BASE
 *    +-- doc/overview.edoc
 *    +-- ebin ([ARTIFACTID].app, [ARTIFACTID].appup)
 *    +-- include (*.hrl)
 *    +-- priv (*)
 *    +-- src (*.erl, private *.hrl)
 *    +-- test_include (*.hrl)
 *    +-- test_priv (*)
 *    +-- test_src (*.erl)
 *    +-- test (*.erl)
 *    +-- target (build artifacts)
 *    +-- pom.xml
 * </pre>
 * 
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 * @since 2.1.0
 */
public class ErlangStdLayout implements SourceLayout {
  private final MavenProject project;
  private final File base;

  public ErlangStdLayout(MavenProject project) {
    this.project = project;
    this.base = project.getBasedir();
  }

  @Override
  public File base() {
    return this.base;
  }

  @Override
  public File ebin() {
    return new File(this.base, "ebin");
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
    return new File(this.base, "include");
  }

  @Override
  public File priv() {
    return new File(this.base, "priv");
  }

  @Override
  public File src() {
    return new File(this.base, "src");
  }

  @Override
  public File testInclude() {
    return new File(this.base, "test_include");
  }

  @Override
  public File testPriv() {
    return new File(this.base, "test_priv");
  }

  @Override
  public Collection<File> testSrcs() {
    return Arrays.asList(src(), new File(this.base, "test_src"), new File(this.base, "test"));
  }

  @Override
  public File overviewEdoc() {
    return new File(new File(this.base, "doc"), ErlConstants.OVERVIEW_EDOC);
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
