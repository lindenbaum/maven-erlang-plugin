package eu.lindenbaum.maven.mojo.app;

import java.io.File;
import java.io.FileFilter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import eu.lindenbaum.maven.ErlangMojo;
import eu.lindenbaum.maven.Properties;
import eu.lindenbaum.maven.util.ErlConstants;
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
    Collection<File> current;

    FileUtils.removeDirectory(p.targetSrc());
    current = FileUtils.copyDirectory(p.src(), p.targetSrc(), FileUtils.SOURCE_FILTER);
    if (current.size() > 0) {
      log.debug("Copied sources:");
      MavenUtils.logCollection(log, LogLevel.DEBUG, current, " * ");
    }
    else {
      FileUtils.removeEmptyDirectory(p.targetSrc());
    }

    FileUtils.removeDirectory(p.targetInclude());
    current = FileUtils.copyDirectory(p.include(), p.targetInclude(), FileUtils.SOURCE_FILTER);
    if (current.size() > 0) {
      log.debug("Copied includes:");
      MavenUtils.logCollection(log, LogLevel.DEBUG, current, " * ");
    }
    else {
      FileUtils.removeEmptyDirectory(p.targetInclude());
    }

    FileUtils.removeDirectory(p.targetPriv());
    current = FileUtils.copyDirectory(p.priv(), p.targetPriv(), FileUtils.NULL_FILTER);
    if (current.size() > 0) {
      log.debug("Copied resources:");
      MavenUtils.logCollection(log, LogLevel.DEBUG, current, " * ");
    }
    else {
      FileUtils.removeEmptyDirectory(p.targetPriv());
    }

    current = new ArrayList<File>();
    for (Artifact artifact : MavenUtils.getForeignDependenciesToPackage(p.project())) {
      File source = artifact.getFile();
      File destination = new File(p.targetPriv(), source.getName());
      try {
        org.codehaus.plexus.util.FileUtils.copyFile(source, destination);
        current.add(source);
      }
      catch (IOException e) {
        log.error("Failed to copy artifact " + source.getPath() + " to " + p.targetPriv() + ".", e);
      }
    }
    if (current.size() > 0) {
      log.debug("Copied foreign artifacts:");
      MavenUtils.logCollection(log, LogLevel.DEBUG, current, " * ");
    }

    Map<String, String> replacements = new HashMap<String, String>();
    replacements.put("${ARTIFACT}", p.project().getArtifactId());
    replacements.put("${DESCRIPTION}", p.project().getDescription());
    replacements.put("${ID}", p.project().getId());
    replacements.put("${VERSION}", p.project().getVersion());
    current = FileUtils.copyDirectory(p.site(), p.target(), new FileFilter() {
      @Override
      public boolean accept(File pathname) {
        return pathname.isFile() && pathname.getName().equals(ErlConstants.OVERVIEW_EDOC);
      }
    }, replacements);
    if (current.size() > 0) {
      log.debug("Copied documentation resources:");
      MavenUtils.logCollection(log, LogLevel.DEBUG, current, " * ");
    }
  }
}
