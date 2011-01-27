package eu.lindenbaum.maven.mojo.app;

import static eu.lindenbaum.maven.util.FileUtils.copyDirectory;
import static eu.lindenbaum.maven.util.FileUtils.removeDirectory;

import java.io.File;
import java.io.FileFilter;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import eu.lindenbaum.maven.ErlangMojo;
import eu.lindenbaum.maven.Properties;
import eu.lindenbaum.maven.util.ErlConstants;
import eu.lindenbaum.maven.util.FileUtils;
import eu.lindenbaum.maven.util.MavenUtils;

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
    removeDirectory(p.targetProject());

    int sources = 0;
    sources += copyDirectory(p.src(), p.targetSrc(), FileUtils.SOURCE_FILTER);
    log.debug("copied " + sources + " sources");
    if (sources == 0) {
      p.targetSrc().delete();
    }

    int includes = 0;
    includes += copyDirectory(p.include(), p.targetInclude(), FileUtils.SOURCE_FILTER);
    log.debug("copied " + includes + " includes");
    if (includes == 0) {
      p.targetInclude().delete();
    }

    int resources = 0;
    resources += copyDirectory(p.priv(), p.targetPriv(), FileUtils.NULL_FILTER);
    log.debug("copied " + resources + " resources");
    if (resources == 0) {
      p.targetPriv().delete();
    }

    int foreignArtifacts = 0;
    for (Artifact artifact : MavenUtils.getForeignDependenciesToPackage(p.project())) {
      File source = artifact.getFile();
      File destination = new File(p.targetPriv(), source.getName());
      try {
        org.codehaus.plexus.util.FileUtils.copyFile(source, destination);
        foreignArtifacts++;
      }
      catch (IOException e) {
        log.error("Failed to copy artifact " + source.getPath() + " to " + p.targetPriv() + ".", e);
      }
    }
    log.debug("copied " + foreignArtifacts + " foreign artifacts");

    Map<String, String> replacements = new HashMap<String, String>();
    replacements.put("${ARTIFACT}", p.project().getArtifactId());
    replacements.put("${DESCRIPTION}", p.project().getDescription());
    replacements.put("${ID}", p.project().getId());
    replacements.put("${VERSION}", p.project().getVersion());
    int overview = 0;
    overview += copyDirectory(p.site(), p.target(), new FileFilter() {
      @Override
      public boolean accept(File pathname) {
        return pathname.isFile() && pathname.getName().equals(ErlConstants.OVERVIEW_EDOC);
      }
    }, replacements);
    log.debug("copied " + overview + " overview.edoc file(s)");
  }
}
