package eu.lindenbaum.maven.mojo.app;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import eu.lindenbaum.maven.ErlangMojo;
import eu.lindenbaum.maven.Properties;
import eu.lindenbaum.maven.util.FileUtils;
import eu.lindenbaum.maven.util.MavenUtils;
import eu.lindenbaum.maven.util.MavenUtils.LogLevel;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;

/**
 * Copies all resource files into that target directory structure. Copied
 * resources contain:
 * <ul>
 * <li>erlang source files (*.erl)</li>
 * <li>erlang include files (*.hrl)</li>
 * <li>edoc overviews (overview.edoc)</li>
 * <li>resources (*)</li>
 * </ul>
 * 
 * @goal generate-resources
 * @phase generate-resources
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 */
public final class ResourceGenerator extends ErlangMojo {
  @Override
  protected void execute(Log log, Properties p) throws MojoExecutionException {
    copySources(log, p);
    copyIncludes(log, p);
    copyResources(log, p);
    copyNonErlangDependencies(log, p);
    copyDocumentationResources(log, p);
  }

  private static void copySources(Log log, Properties p) throws MojoExecutionException {
    File src = p.sourceLayout().src();
    File targetSrc = p.targetLayout().src();
    FileUtils.removeDirectory(targetSrc);
    Collection<File> current = FileUtils.copyDirectory(src, targetSrc, FileUtils.SOURCE_FILTER);
    if (current.size() > 0) {
      log.debug("Copied sources:");
      MavenUtils.logCollection(log, LogLevel.DEBUG, current, " * ");
    }
    else {
      FileUtils.removeEmptyDirectory(targetSrc);
    }
  }

  private static void copyIncludes(Log log, Properties p) throws MojoExecutionException {
    File include = p.sourceLayout().include();
    File targetInclude = p.targetLayout().include();
    FileUtils.removeDirectory(targetInclude);
    Collection<File> current = FileUtils.copyDirectory(include, targetInclude, FileUtils.SOURCE_FILTER);
    if (current.size() > 0) {
      log.debug("Copied includes:");
      MavenUtils.logCollection(log, LogLevel.DEBUG, current, " * ");
    }
    else {
      FileUtils.removeEmptyDirectory(targetInclude);
    }
  }

  private static void copyResources(Log log, Properties p) throws MojoExecutionException {
    File priv = p.sourceLayout().priv();
    File targetPriv = p.targetLayout().priv();
    FileUtils.removeDirectory(targetPriv);
    Collection<File> current = FileUtils.copyDirectory(priv, targetPriv, FileUtils.NULL_FILTER);
    if (current.size() > 0) {
      log.debug("Copied resources:");
      MavenUtils.logCollection(log, LogLevel.DEBUG, current, " * ");
    }
    else {
      FileUtils.removeEmptyDirectory(targetPriv);
    }
  }

  private static void copyNonErlangDependencies(Log log, Properties p) {
    File targetPriv = p.targetLayout().priv();
    Collection<File> current = new ArrayList<File>();
    for (Artifact artifact : MavenUtils.getForeignDependenciesToPackage(p.project())) {
      File source = artifact.getFile();
      File destination = new File(targetPriv, source.getName());
      try {
        org.codehaus.plexus.util.FileUtils.copyFile(source, destination);
        current.add(source);
      }
      catch (IOException e) {
        log.error("Failed to copy artifact " + source.getPath() + " to " + targetPriv + ".", e);
      }
    }
    if (current.size() > 0) {
      log.debug("Copied foreign artifacts:");
      MavenUtils.logCollection(log, LogLevel.DEBUG, current, " * ");
    }
  }

  private static void copyDocumentationResources(Log log, Properties p) throws MojoExecutionException {
    Map<String, String> replacements = new HashMap<String, String>();
    replacements.put("${ARTIFACT}", p.project().getArtifactId());
    replacements.put("${NAME}", p.project().getName());
    replacements.put("${DESCRIPTION}", p.project().getDescription());
    replacements.put("${ID}", p.project().getId());
    replacements.put("${VERSION}", p.project().getVersion());

    File overviewEdoc = p.sourceLayout().overviewEdoc();
    if (overviewEdoc.isFile()) {
      File targetOverviewEdoc = p.targetLayout().overviewEdoc();
      FileUtils.copyFile(overviewEdoc, targetOverviewEdoc, replacements);
      log.debug("Copied documentation resources:");
      log.debug(" * " + overviewEdoc);
    }
  }
}
