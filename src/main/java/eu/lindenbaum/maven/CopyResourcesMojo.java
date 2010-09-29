package eu.lindenbaum.maven;

import static eu.lindenbaum.maven.util.ErlConstants.APP_SUFFIX;
import static eu.lindenbaum.maven.util.ErlConstants.ERL_SUFFIX;
import static eu.lindenbaum.maven.util.FileUtils.APP_FILTER;
import static eu.lindenbaum.maven.util.FileUtils.NULL_FILTER;
import static eu.lindenbaum.maven.util.FileUtils.copyDirectory;
import static eu.lindenbaum.maven.util.FileUtils.getFilesRecursive;
import static org.codehaus.plexus.util.FileUtils.fileWrite;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.maven.plugin.Mojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;

/**
 * <p>
 * This {@link Mojo} copies the private resources to the
 * {@link AbstractErlangMojo#targetPriv} directory. Default source folders for
 * private resources are {@link AbstractErlangMojo#srcMainPriv} and
 * {@link AbstractErlangMojo#srcMainResources}.
 * </p>
 * <p>
 * Besides that this {@link Mojo} also copies the erlang application resource
 * file. In order to manage the project over the project pom there is the
 * possibility to let the {@link Mojo} automatically fill in values from the
 * project pom into the {@code .app} file. This can be done by placing
 * placeholders into the application resource files. Below is a list of
 * possible placeholders and their substitutions:
 * </p>
 * <ul>
 * <li>{@code ?APP}: the projects artifact id</li>
 * <li>{@code ?DESCRIPTION}: the projects description</li>
 * <li>{@code ?ID}: the projects id</li>
 * <li>{@code ?VERSION}: the projects version</li>
 * <li>{@code ?MODULES}: all {@code .erl} files found in the source folder</li>
 * </ul>
 * <p>
 * In case there is no application resouce file specified the {@link Mojo} will
 * try to generate a default {@code .app} file for you. A default app file
 * would look like this: 
 * </p>
 * <pre>
 * {application, ?APP,
 *   [{description, ?DESCRIPTION},
 *    {id, ?ID},
 *    {vsn, ?VERSION},
 *    {modules, ?MODULES},
 *    {maxT, infinity},
 *    {registered, []},
 *    {included_applications, []},
 *    {applications, []},
 *    {env, []},
 *    {mod, []},
 *    {start_phases, []}]}.
 * </pre>
 * <p>
 * TODO the application resource file generation/processing should be moved to
 * the {@link PackageMojo}.
 * </p>
 * 
 * @goal copy-resources
 * @phase process-resources
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 */
public final class CopyResourcesMojo extends AbstractErlangMojo {
  /**
   * A default app file managed by this {@link Mojo} containing all possible
   * replacements.
   */
  private static final String DEFAULT_APP = "{application, ?APP,\n" //
                                            + "  [{description,  ?DESCRIPTION},\n" //
                                            + "   {id, ?ID},\n" //
                                            + "   {vsn, ?VERSION},\n" //
                                            + "   {modules, ?MODULES},\n" //
                                            + "   {maxT, infinity},\n" //
                                            + "   {registered, []},\n" //
                                            + "   {included_applications, []},\n" //
                                            + "   {applications, []},\n" //
                                            + "   {env, []},\n" //
                                            + "   {mod, []},\n" //
                                            + "   {start_phases, []}]}.\n";

  @Override
  public void execute() throws MojoExecutionException {
    Log log = getLog();

    Map<String, String> replacements = new HashMap<String, String>();
    replacements.put("?APP", "\'" + this.project.getArtifactId() + "\'");
    replacements.put("?DESCRIPTION", "\"" + this.project.getDescription() + "\"");
    replacements.put("?ID", "\"" + this.project.getId() + "\"");
    replacements.put("?VERSION", "\"" + this.project.getVersion() + "\"");
    replacements.put("?MODULES", getModules(this.srcMainErlang));

    List<File> appFiles = getFilesRecursive(this.srcMainErlang, APP_SUFFIX);
    if (appFiles != null && appFiles.size() == 0) {
      File appFile = new File(this.srcMainErlang, this.project.getArtifactId() + APP_SUFFIX);
      try {
        fileWrite(appFile.getPath(), "UTF-8", DEFAULT_APP);
        appFile.deleteOnExit();
      }
      catch (IOException e) {
        log.info("Could not find .app file, also failed to create default .app file, ignoring ...");
      }
    }

    int copiedFiles = 0;
    copiedFiles += copyDirectory(this.srcMainPriv, this.targetPriv, NULL_FILTER);
    copiedFiles += copyDirectory(this.srcMainResources, this.targetPriv, NULL_FILTER);
    copiedFiles += copyDirectory(this.srcMainErlang, this.targetEbin, APP_FILTER, replacements);
    log.info("Copied " + copiedFiles + " files");
  }

  /**
   * Returns a {@link String} containing all source modules of in the given
   * directory.
   * 
   * @param directory to scan for sources
   * @return a {@link String} containing all found source modules
   */
  private static String getModules(File directory) {
    StringBuilder modules = new StringBuilder("[");
    List<File> sources = getFilesRecursive(directory, ERL_SUFFIX);
    for (int i = 0; i < sources.size(); ++i) {
      if (i != 0) {
        modules.append(", ");
      }
      modules.append(sources.get(i).getName().replace(ERL_SUFFIX, ""));
    }
    modules.append("]");
    return modules.toString();
  }
}
