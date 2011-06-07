package eu.lindenbaum.maven;

import java.io.File;
import java.util.Collection;
import java.util.Collections;

import eu.lindenbaum.maven.util.ErlConstants;

import org.apache.maven.project.MavenProject;

/**
 * Implementation of the {@link SourceLayout} interface for projects with
 * packaging type {@link PackagingType#ERLANG_REL}. Packaging type for
 * erlang/OTP releases with trivial directory layout:
 * 
 * <pre>
 *  BASE
 *    +-- target (build artifacts)
 *    +-- pom.xml
 *    +-- [ARTIFACTID].rel
 *    +-- sys.config
 *    +-- relup
 * </pre>
 * 
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 * @since 2.1.0
 */
public class ErlangRelLayout implements SourceLayout {
  private final MavenProject project;
  private final File base;

  public ErlangRelLayout(MavenProject project) {
    this.project = project;
    this.base = project.getBasedir();
  }

  @Override
  public File base() {
    return this.base;
  }

  @Override
  public File ebin() {
    return null;
  }

  @Override
  public File appFile() {
    return null;
  }

  @Override
  public File appupFile() {
    return null;
  }

  @Override
  public File include() {
    return null;
  }

  @Override
  public File priv() {
    return null;
  }

  @Override
  public File src() {
    return null;
  }

  @Override
  public File testInclude() {
    return null;
  }

  @Override
  public File testPriv() {
    return null;
  }

  @Override
  public Collection<File> testSrcs() {
    return Collections.emptyList();
  }

  @Override
  public File overviewEdoc() {
    return null;
  }

  @Override
  public File relFile() {
    return new File(this.base, this.project.getArtifactId() + ErlConstants.REL_SUFFIX);
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
