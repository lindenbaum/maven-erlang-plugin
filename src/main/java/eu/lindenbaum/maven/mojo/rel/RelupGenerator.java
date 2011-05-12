package eu.lindenbaum.maven.mojo.rel;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import eu.lindenbaum.maven.ErlangMojo;
import eu.lindenbaum.maven.PackagingType;
import eu.lindenbaum.maven.Properties;
import eu.lindenbaum.maven.archiver.TarGzUnarchiver;
import eu.lindenbaum.maven.erlang.GenericScriptResult;
import eu.lindenbaum.maven.erlang.MakeRelupScript;
import eu.lindenbaum.maven.erlang.MavenSelf;
import eu.lindenbaum.maven.erlang.Script;
import eu.lindenbaum.maven.util.ErlConstants;
import eu.lindenbaum.maven.util.FileUtils;
import eu.lindenbaum.maven.util.MavenUtils;
import eu.lindenbaum.maven.util.MavenUtils.LogLevel;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.VersionRange;
import org.apache.maven.plugin.Mojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugin.logging.Log;

/**
 * This {@link Mojo} will generate a release upgrade file template for a release
 * project. In order to do so all available non-SNAPSHOT versions of the project
 * will be resolved (also from remote repositories). All versions smaller than
 * the current version will be included in the generation. The resulting release
 * upgrade file's location will be prompted on success as well as its content.
 * The actual generation will be made using <code>systools:make_relup/4</code>.
 * 
 * @goal relup
 * @since 2.1.0
 * @execute phase="generate-resources" lifecycle="appup-relup"
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 * @see <a href="http://www.erlang.org/doc/man/relup.html"
 *      >http://www.erlang.org/doc/man/relup.html</a>
 */
public final class RelupGenerator extends ErlangMojo {
  @Override
  protected void execute(Log log, Properties p) throws MojoExecutionException, MojoFailureException {
    log.info(MavenUtils.SEPARATOR);
    log.info(" R E L U P - G E N E R A T O R");
    log.info(MavenUtils.SEPARATOR);

    PackagingType packagingType = p.packagingType();
    if (packagingType != PackagingType.ERLANG_REL) {
      throw new MojoExecutionException("Mojo does not support packaging type " + packagingType + ".");
    }

    List<String> releases = new ArrayList<String>();
    List<File> paths = new ArrayList<File>();
    paths.add(new File(new File(p.targetLib(), "*"), "ebin"));
    Artifact artifact = p.project().getArtifact();
    Set<ArtifactVersion> availableVersions = MavenUtils.getAvailableVersions(artifact, p.components());

    // process earlier non-snapshot versions
    VersionRange range = MavenUtils.createVersionRange("(, " + artifact.getVersion() + ")");
    for (ArtifactVersion artifactVersion : availableVersions) {
      if (range.containsVersion(artifactVersion)) {
        Artifact toResolve = MavenUtils.getArtifact(artifact, artifactVersion.toString(), p.components());
        if (!toResolve.isSnapshot()) {
          String releaseName = getReleaseName(toResolve);
          File destination = new File(p.target(), releaseName);
          TarGzUnarchiver unarchiver = new TarGzUnarchiver(p.node(), p.cookie(), destination);
          extract(unarchiver, MavenUtils.getArtifactFile(toResolve, p.components()));

          // put release file into path for systools
          File relFile = new File(new File(destination, "releases"), releaseName + ErlConstants.REL_SUFFIX);
          FileUtils.copyFiles(p.target(), relFile);

          // add systools path for the release
          paths.add(new File(new File(new File(destination, "lib"), "*"), "ebin"));
          releases.add(releaseName);
        }
      }
    }

    File relupFile = p.targetRelupFile();
    File relFile = p.targetRelFile();
    Script<GenericScriptResult> script = new MakeRelupScript(relupFile, relFile, releases, paths);
    GenericScriptResult result = MavenSelf.get(p.cookie()).exec(p.node(), script);
    if (result.success()) {
      log.info("Successfully generated release upgrade file.");
      MavenUtils.logContent(log, LogLevel.INFO, relupFile, "");
    }
    else {
      result.logOutput(log);
      throw new MojoExecutionException("Failed to create release upgrade file.");
    }
  }

  /**
   * Wraps extraction of an tar gz archive.
   */
  private static void extract(TarGzUnarchiver unarchiver, File file) throws MojoExecutionException {
    try {
      unarchiver.extract(file);
    }
    catch (IOException e) {
      throw new MojoExecutionException("Failed to extract " + file + ".", e);
    }
  }

  /**
   * Returns the base name of a certain artifact.
   */
  private static String getReleaseName(Artifact artifact) {
    return artifact.getArtifactId() + "-" + artifact.getBaseVersion();
  }
}
