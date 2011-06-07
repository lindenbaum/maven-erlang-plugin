package eu.lindenbaum.maven;

import java.util.List;

import org.apache.maven.artifact.factory.ArtifactFactory;
import org.apache.maven.artifact.metadata.ArtifactMetadataSource;
import org.apache.maven.artifact.repository.ArtifactRepository;
import org.apache.maven.artifact.resolver.ArtifactResolver;

/**
 * Implementation of the {@link MavenComponents} bean that provides maven
 * components to be used by the plugin.
 * 
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 */
public class DefaultMavenComponents implements MavenComponents {
  private final ArtifactRepository localRepository;
  private final List<ArtifactRepository> remoteRepositories;
  private final ArtifactMetadataSource metadataSource;
  private final ArtifactFactory artifactFactory;
  private final ArtifactResolver artifactResolver;

  public DefaultMavenComponents(ArtifactRepository localRepository,
                                List<ArtifactRepository> remoteRepositories,
                                ArtifactMetadataSource metadataSource,
                                ArtifactFactory artifactFactory,
                                ArtifactResolver artifactResolver) {
    this.localRepository = localRepository;
    this.remoteRepositories = remoteRepositories;
    this.metadataSource = metadataSource;
    this.artifactFactory = artifactFactory;
    this.artifactResolver = artifactResolver;
  }

  @Override
  public ArtifactRepository localRepository() {
    return this.localRepository;
  }

  @Override
  public List<ArtifactRepository> remoteRepositories() {
    return this.remoteRepositories;
  }

  @Override
  public ArtifactMetadataSource metadataSource() {
    return this.metadataSource;
  }

  @Override
  public ArtifactFactory artifactFactory() {
    return this.artifactFactory;
  }

  @Override
  public ArtifactResolver artifactResolver() {
    return this.artifactResolver;
  }
}
