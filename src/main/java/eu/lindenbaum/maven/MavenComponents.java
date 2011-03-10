package eu.lindenbaum.maven;

import java.util.List;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.factory.ArtifactFactory;
import org.apache.maven.artifact.metadata.ArtifactMetadataSource;
import org.apache.maven.artifact.repository.ArtifactRepository;
import org.apache.maven.artifact.resolver.ArtifactResolver;
import org.apache.maven.project.MavenProject;

/**
 * Represents a bean interface holding all maven components the plugin uses.
 * 
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 */
public interface MavenComponents {
  /**
   * Returns the local {@link ArtifactRepository} storing dependencies of this
   * {@link MavenProject}.
   */
  public ArtifactRepository localRepository();

  /**
   * Returns a list of remote {@link ArtifactRepository} storing dependencies of
   * this {@link MavenProject}.
   */
  public List<ArtifactRepository> remoteRepositories();

  /**
   * Returns the {@link ArtifactMetadataSource} used to query metadata
   * information for {@link Artifact}s.
   */
  public ArtifactMetadataSource metadataSource();

  /**
   * Returns a factory that can be used create {@link Artifact} objects.
   */
  public ArtifactFactory artifactFactory();

  /**
   * Returns an {@link ArtifactResolver} that can resolve/download artifacts
   * from remote repositories into the local repository.
   */
  public ArtifactResolver artifactResolver();
}
