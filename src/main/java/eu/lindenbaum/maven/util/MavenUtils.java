package eu.lindenbaum.maven.util;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import eu.lindenbaum.maven.PackagingType;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.DefaultArtifact;
import org.apache.maven.artifact.handler.ArtifactHandler;
import org.apache.maven.artifact.repository.ArtifactRepository;
import org.apache.maven.artifact.versioning.VersionRange;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.project.MavenProject;

/**
 * Containing utilities related to maven plugins/projects.
 * 
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 * @author Olle Törnström <olle.toernstroem@lindenbaum.eu>
 */
public final class MavenUtils {
  public static String SEPARATOR = "------------------------------------------------------------------------";

  /**
   * Returns an (existing) file pointing to a plugin {@link Artifact} used by a
   * {@link MavenProject} from a {@link ArtifactRepository}.
   * 
   * @param artifactId to lookup in the project
   * @param project to scan for the plugin {@link Artifact}
   * @param repository to scan for the artifact
   * @return an existing file pointing to a plugin artifact
   * @throws MojoExecutionException
   */
  public static File getPluginFile(String artifactId, MavenProject project, ArtifactRepository repository) throws MojoExecutionException {
    @SuppressWarnings("unchecked")
    Set<Artifact> plugins = project.getPluginArtifacts();
    Artifact resolved = null;
    for (Artifact artifact : plugins) {
      if (artifact.getArtifactId().equals(artifactId)) {
        resolved = artifact;
        break;
      }
    }
    if (resolved != null) {
      return getArtifactFile(resolved, repository);
    }
    else {
      throw new MojoExecutionException("project does not use a plugin with artifact id " + artifactId);
    }
  }

  /**
   * Returns an (existing) file pointing to an {@link Artifact} of an
   * {@link ArtifactRepository}.
   * 
   * @param artifact to lookup in the repository
   * @param repository to scan for the artifact
   * @return an existing file pointing to an artifact
   * @throws MojoExecutionException
   */
  public static File getArtifactFile(Artifact artifact, ArtifactRepository repository) throws MojoExecutionException {
    File file = new File(repository.getBasedir(), repository.pathOf(artifact));
    if (file.exists()) {
      return file;
    }
    throw new MojoExecutionException("artifact " + artifact + " not present in the repository");
  }

  /**
   * Returns an {@link Artifact} object of a specific {@link Artifact} with a
   * specific version.
   * 
   * @param from to clone
   * @param version of the returned artifact
   * @return a new {@link Artifact} with the requested version
   */
  public static Artifact getArtifact(Artifact from, String version) {
    String groupId = from.getGroupId();
    String artifactId = from.getArtifactId();
    String scope = from.getScope();
    String type = from.getType();
    String classifier = from.getClassifier();
    ArtifactHandler artifactHandler = from.getArtifactHandler();
    VersionRange versionRange = VersionRange.createFromVersion(version);
    return new DefaultArtifact(groupId, artifactId, versionRange, scope, type, classifier, artifactHandler);
  }

  /**
   * Returns the release name for the given {@link Artifact}. The release name
   * consists of the artifacts id and its version.
   * 
   * @param artifact to retrieve the release name from
   * @return a string containing the release name
   */
  public static String getReleaseName(Artifact artifact) {
    return artifact.getArtifactId() + "-" + artifact.getVersion();
  }

  /**
   * Returns the transitive erlang artifacts of a project using
   * {@link MavenProject#getArtifacts()} filtered for
   * {@link PackagingType#ERLANG_OTP} and {@link PackagingType#ERLANG_STD}
   * packaged projects. This will return all {@link Artifact} with scopes other
   * than {@code test} and {@code provided}.
   * 
   * @param project to get the dependencies for
   * @return a non-{@code null} {@link List} of dependency artifacts
   */
  public static List<Artifact> getErlangReleaseArtifacts(MavenProject project) {
    ArrayList<Artifact> result = new ArrayList<Artifact>();
    for (Artifact artifact : getErlangArtifacts(project)) {
      String scope = artifact.getScope();
      if (!"test".equals(scope) && !"provided".equals(scope)) {
        result.add(artifact);
      }
    }
    return result;
  }

  /**
   * Returns the direct erlang dependencies of a project using
   * {@link MavenProject#getDependencyArtifacts()} filtered for
   * {@link PackagingType#ERLANG_OTP} and {@link PackagingType#ERLANG_STD}
   * packaged projects. This will return all {@link Artifact} with scopes other
   * than {@code test} and {@code provided}.
   * 
   * @param project to get the dependencies for
   * @return a non-{@code null} {@link List} of dependency artifacts
   */
  public static List<Artifact> getErlangDependenciesToPackage(MavenProject project) {
    ArrayList<Artifact> result = new ArrayList<Artifact>();
    for (Artifact artifact : getErlangDependencies(project)) {
      String scope = artifact.getScope();
      if (!"test".equals(scope) && !"provided".equals(scope)) {
        result.add(artifact);
      }
    }
    return result;
  }

  /**
   * Returns the transitive erlang artifacts of a project using
   * {@link MavenProject#getArtifacts()} filtered for
   * {@link PackagingType#ERLANG_OTP} and {@link PackagingType#ERLANG_STD}
   * packaged projects.
   * 
   * @param project to get the dependencies for
   * @return a non-{@code null} {@link List} of dependency artifacts
   */
  public static List<Artifact> getErlangArtifacts(MavenProject project) {
    ArrayList<Artifact> result = new ArrayList<Artifact>();
    for (Artifact artifact : getArtifacts(project)) {
      String type = artifact.getType();
      if (PackagingType.ERLANG_OTP.isA(type) || PackagingType.ERLANG_STD.isA(type)) {
        result.add(artifact);
      }
    }
    return result;
  }

  /**
   * Returns the direct erlang dependencies of a project using
   * {@link MavenProject#getDependencyArtifacts()} filtered for
   * {@link PackagingType#ERLANG_OTP} and {@link PackagingType#ERLANG_STD}
   * packaged projects.
   * 
   * @param project to get the dependencies for
   * @return a non-{@code null} {@link List} of dependency artifacts
   */
  public static List<Artifact> getErlangDependencies(MavenProject project) {
    ArrayList<Artifact> result = new ArrayList<Artifact>();
    for (Artifact artifact : getDependencies(project)) {
      String type = artifact.getType();
      if (PackagingType.ERLANG_OTP.isA(type) || PackagingType.ERLANG_STD.isA(type)) {
        result.add(artifact);
      }
    }
    return result;
  }

  /**
   * Returns the direct non-erlang dependencies of a project using
   * {@link MavenProject#getDependencyArtifacts()}. All erlang artifacts will be
   * filtered out. This will return all {@link Artifact} with scopes other than
   * {@code test} and {@code provided}.
   * 
   * @param project to get the dependencies for
   * @return a non-{@code null} {@link List} of dependency artifacts
   */
  public static List<Artifact> getForeignDependenciesToPackage(MavenProject project) {
    ArrayList<Artifact> result = new ArrayList<Artifact>();
    for (Artifact artifact : getForeignDependencies(project)) {
      String scope = artifact.getScope();
      if (!"test".equals(scope) && !"provided".equals(scope)) {
        result.add(artifact);
      }
    }
    return result;
  }

  /**
   * Returns the direct non-erlang dependencies of a project using
   * {@link MavenProject#getDependencyArtifacts()}. All erlang artifacts will be
   * filtered out.
   * 
   * @param project to get the dependencies for
   * @return a non-{@code null} {@link List} of dependency artifacts
   */
  public static List<Artifact> getForeignDependencies(MavenProject project) {
    ArrayList<Artifact> result = new ArrayList<Artifact>();
    for (Artifact artifact : getDependencies(project)) {
      String type = artifact.getType();
      if (!PackagingType.ERLANG_OTP.isA(type) //
          && !PackagingType.ERLANG_STD.isA(type) //
          && !PackagingType.ERLANG_REL.isA(type)) {
        result.add(artifact);
      }
    }
    return result;
  }

  /**
   * Returns the direct dependency artifacts of a project using
   * {@link MavenProject#getDependencyArtifacts()}.
   * 
   * @param project to get the dependencies for
   * @return a non-{@code null} {@link List} of dependency artifacts
   */
  public static List<Artifact> getDependencies(MavenProject project) {
    @SuppressWarnings("unchecked")
    Set<Artifact> artifacts = project.getDependencyArtifacts();
    return new ArrayList<Artifact>(artifacts);
  }

  /**
   * Returns the transitive dependency artifacts of a project using
   * {@link MavenProject#getArtifacts()}.
   * 
   * @param project to get the dependencies for
   * @return a non-{@code null} {@link List} of dependency artifacts
   */
  public static List<Artifact> getArtifacts(MavenProject project) {
    @SuppressWarnings("unchecked")
    Set<Artifact> artifacts = project.getArtifacts();
    return new ArrayList<Artifact>(artifacts);
  }
}
