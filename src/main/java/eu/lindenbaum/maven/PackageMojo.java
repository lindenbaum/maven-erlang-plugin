package eu.lindenbaum.maven;

import static eu.lindenbaum.maven.util.ErlConstants.APPUP_SUFFIX;
import static eu.lindenbaum.maven.util.ErlConstants.APP_SUFFIX;
import static eu.lindenbaum.maven.util.ErlConstants.BEAM_SUFFIX;
import static eu.lindenbaum.maven.util.ErlConstants.SRC_SUFFIX;
import static eu.lindenbaum.maven.util.ErlConstants.TARGZ_SUFFIX;
import static eu.lindenbaum.maven.util.ErlUtils.eval;
import static eu.lindenbaum.maven.util.FileUtils.APP_FILTER;
import static eu.lindenbaum.maven.util.FileUtils.NULL_FILTER;
import static eu.lindenbaum.maven.util.FileUtils.SNMP_FILTER;
import static eu.lindenbaum.maven.util.FileUtils.SOURCE_FILTER;
import static eu.lindenbaum.maven.util.FileUtils.copyDirectory;
import static eu.lindenbaum.maven.util.FileUtils.getDependencies;
import static eu.lindenbaum.maven.util.FileUtils.getDirectoriesRecursive;
import static eu.lindenbaum.maven.util.FileUtils.getFilesRecursive;
import static eu.lindenbaum.maven.util.FileUtils.removeDirectory;
import static org.codehaus.plexus.util.FileUtils.fileWrite;

import java.io.File;
import java.io.FileFilter;
import java.io.IOException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import eu.lindenbaum.maven.util.TarGzArchiver;

import org.apache.maven.plugin.Mojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugin.logging.Log;

/**
 * <p>
 * This {@link Mojo} packages all application artifacts into a single
 * {@code .tar.gz} package. This includes {@code .beam} files, the {@code .hrl}
 * include files, SNMP resources, private data from the {@code priv} and
 * {@code resources} directories and non-erlang sources.
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
 * <li>{@code ?ARTIFACT}: the projects artifact id</li>
 * <li>{@code ?DESCRIPTION}: the projects description</li>
 * <li>{@code ?ID}: the projects id</li>
 * <li>{@code ?VERSION}: the projects version</li>
 * <li>{@code ?MODULES}: all compiled {@code .beam} files found in the target ebin folder</li>
 * <li>{@code ?REGISTERED}: a list of registered names, based on the
 * {@code -registered(Names).} attribute retrieved from the compiled {@code .beam}
 * files.</li>
 * </ul>
 * <p>
 * In case there is no application resouce file specified the {@link Mojo} will
 * try to generate a default {@code .app} file for you. A default app file
 * would look like this: 
 * </p>
 * <pre>
 * {application, ?ARTIFACT,
 *   [{description, ?DESCRIPTION},
 *    {id, ?ID},
 *    {vsn, ?VERSION},
 *    {modules, ?MODULES},
 *    {maxT, infinity},
 *    {registered, ?REGISTERED},
 *    {included_applications, []},
 *    {applications, []},
 *    {env, []},
 *    {mod, []},
 *    {start_phases, []}]}.
 * </pre>
 * <p>
 * In case the user decides to provide its own application resource and upgrade
 * files these will be checked for plausability by checking the application
 * version against the project version and checking the application modules
 * against the found, compiled modules.
 * </p>
 * 
 * @goal package
 * @phase package
 * @author Olivier Sambourg
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 */
public final class PackageMojo extends AbstractErlangMojo {
  private static final String EXTRACT_VERSION = //
  "{ok, List} = file:consult(\"%s\"), " //
      + "{value, {application, %s, Properties}} = lists:keysearch(%s, 2, List), " //
      + "{value, {vsn, Version}} = lists:keysearch(vsn, 1, Properties), " //
      + "io:format(Version), io:nl().";

  private static final String EXTRACT_MODULES = //
  "{ok, List} = file:consult(\"%s\"), "
      + "{value, {application, %s, Properties}} = lists:keysearch(%s, 2, List), "
      + "{value, {modules, M}} = lists:keysearch(modules, 1, Properties), "
      + "Mods = lists:foldl(fun(Mod, Acc) -> Acc ++ io_lib:format(\"~s \", [Mod]) end, \"\", M), "
      + "io:format(\"~s\", [Mods]), io:nl().";

  private static final String CHECK_APPUP = //
  "{ok, List} = file:consult(\"%s\"), "
      + "{value, {\"%s\", UpFrom, DownTo}} = lists:keysearch(\"%s\", 1, List), "
      + "lists:foreach(fun({Version, ProcList}) -> "
      + "  lists:foreach(fun(ProcElement) -> true = is_tuple(ProcElement) end, ProcList) "
      + "end, UpFrom ++ DownTo)," + "io:format(\"ok\"), io:nl().";

  private static final String EXTRACT_ATTRIBUTE = //
  "Attr = lists:foldl(fun(Module, Acc) ->" //
      + "A = Module:module_info(attributes)," //
      + "case proplists:get_value(%s, A) of" //
      + "  undefined -> Acc;" + "  Registered -> [Registered | Acc]" //
      + "end end, [], %s)," //
      + "io:format(\"~p\", [lists:flatten(Attr)]),io:nl().";

  private static final String DEFAULT_APP = "{application, ?ARTIFACT,\n" //
                                            + "  [{description,  ?DESCRIPTION},\n" //
                                            + "   {id, ?ID},\n" //
                                            + "   {vsn, ?VERSION},\n" //
                                            + "   {modules, ?MODULES},\n" //
                                            + "   {maxT, infinity},\n" //
                                            + "   {registered, ?REGISTERED},\n" //
                                            + "   {included_applications, []},\n" //
                                            + "   {applications, []},\n" //
                                            + "   {env, []},\n" //
                                            + "   {mod, []},\n" //
                                            + "   {start_phases, []}]}.\n";

  /**
   * Setting this to {@code true} will break the build when the application file
   * does not contain all found modules.
   * 
   * @parameter default-value="true"
   */
  private boolean failOnUndeclaredModules;

  @Override
  public void execute() throws MojoExecutionException, MojoFailureException {
    Log log = getLog();
    log.info("------------------------------------------------------------------------");
    log.info("PACKAGING PROJECT");

    File tmpDir = new File(this.target, this.project.getArtifactId() + "-" + this.project.getVersion());
    tmpDir.mkdirs();

    // prepare and copy .app and .appup files
    String modules = getModules(this.targetEbin);
    Map<String, String> replacements = new HashMap<String, String>();
    replacements.put("?ARTIFACT", "\'" + this.project.getArtifactId() + "\'");
    replacements.put("?DESCRIPTION", "\"" + this.project.getDescription() + "\"");
    replacements.put("?ID", "\"" + this.project.getId() + "\"");
    replacements.put("?VERSION", "\"" + this.project.getVersion() + "\"");
    replacements.put("?MODULES", modules);
    replacements.put("?REGISTERED", getRegisteredNames(modules));
    File srcAppFile = new File(this.srcMainErlang, this.project.getArtifactId() + APP_SUFFIX);
    if (!srcAppFile.exists()) {
      try {
        fileWrite(srcAppFile.getPath(), "UTF-8", DEFAULT_APP);
        srcAppFile.deleteOnExit();
      }
      catch (IOException e) {
        log.info("Could not find .app file, also failed to create default .app file, ignoring ...");
      }
    }
    copy(this.srcMainErlang, this.targetEbin, APP_FILTER, "application", replacements);

    // package all otp standard resources
    copy(this.srcMainErlang, new File(tmpDir, "src"), SOURCE_FILTER, "source");
    copy(this.targetEbin, new File(tmpDir, "ebin"), NULL_FILTER, "");
    copy(this.srcMainInclude, new File(tmpDir, "include"), SOURCE_FILTER, "include");
    copy(this.targetPriv, new File(tmpDir, "priv"), NULL_FILTER, "private");
    copy(this.targetMibs, new File(tmpDir, "mibs"), SNMP_FILTER, "SNMP");

    // package non erlang source folders, e.g. c, java, ... into c_src, java_src, ...
    List<File> sources = getDirectoriesRecursive(this.srcMain, new FileFilter() {
      @Override
      public boolean accept(File dir) {
        return !dir.equals(PackageMojo.this.srcMainErlang) && !dir.equals(PackageMojo.this.srcMainInclude)
               && !dir.equals(PackageMojo.this.srcMainPriv) && !dir.equals(PackageMojo.this.srcMainResources);
      }
    });
    for (File source : sources) {
      copy(source, new File(tmpDir, source.getName() + SRC_SUFFIX), NULL_FILTER, "non-erlang source");
    }

    // perform checks on .app and .appup files (if any)
    File appFile = new File(this.targetEbin, this.project.getArtifactId() + APP_SUFFIX);
    if (appFile.exists()) {
      checkVersion(appFile);
      checkModules(appFile);

      File appUpFile = new File(this.targetEbin, this.project.getArtifactId() + APPUP_SUFFIX);
      if (appUpFile.exists()) {
        checkAppUp(appUpFile);
      }
    }
    else {
      getLog().warn("No " + APP_SUFFIX + " file was found");
    }

    // create .tar.gz package
    File toFile = new File(this.target, tmpDir.getName() + TARGZ_SUFFIX);
    try {
      TarGzArchiver archiver = new TarGzArchiver(log, toFile);
      archiver.addFile(tmpDir);
      archiver.createArchive();
      this.project.getArtifact().setFile(toFile);
    }
    catch (Exception e) {
      throw new MojoExecutionException(e.getMessage(), e);
    }
    log.info("------------------------------------------------------------------------");
    removeDirectory(tmpDir);
  }

  /**
   * Copy the content of a directory to another one applying filtering and logging.
   * If the target directory does not exist it will be created.
   * 
   * @param srcDir to copy from
   * @param targetDir to copy to
   * @param filter to apply on found files and directories
   * @param kind identifying the kind of files to copy
   * @throws MojoExecutionException
   */
  private void copy(File srcDir, File targetDir, FileFilter filter, String kind) throws MojoExecutionException {
    copy(srcDir, targetDir, filter, kind, new HashMap<String, String>());
  }

  /**
   * Copy the content of a directory to another one applying filtering and logging.
   * If the target directory does not exist it will be created. If there were no
   * files to copy the target directory will be removed.
   * 
   * @param srcDir to copy from
   * @param targetDir to copy to
   * @param filter to apply on found files and directories
   * @param kind identifying the kind of files to copy
   * @param r replacement strings to be substituted in the copied file(s)
   * @throws MojoExecutionException
   */
  private void copy(File srcDir, File targetDir, FileFilter filter, String kind, Map<String, String> r) throws MojoExecutionException {
    targetDir.mkdirs();
    int copied = copyDirectory(srcDir, targetDir, filter, r);
    if (copied == 0) {
      targetDir.delete();
    }
    getLog().info("Copied " + copied + " " + kind + " files to " + targetDir.getAbsolutePath());
  }

  /**
   * Checks whether the project version equals the erlang application version.
   * 
   * @param appFile the erlang application resource file
   * @throws MojoExecutionException
   * @throws MojoFailureException in case of version mismatch
   */
  private void checkVersion(File appFile) throws MojoExecutionException, MojoFailureException {
    Log log = getLog();
    String name = this.project.getArtifactId();
    String version = this.project.getVersion();
    String expression = String.format(EXTRACT_VERSION, appFile.getPath(), name, name);
    String appFileVersion = eval(log, expression, getDependencies(this.targetLib));
    if (!version.equals(appFileVersion)) {
      log.error("Version mismatch.");
      log.error("Project version is " + version + " while " + APP_SUFFIX + " version is " + appFileVersion);
      throw new MojoFailureException("Version mismatch " + version + " != " + appFileVersion);
    }
  }

  /**
   * Checks whether the modules to be packaged are declared in the erlang
   * application file.
   * 
   * @param appFile the erlang application resource file
   * @throws MojoExecutionException
   * @throws MojoFailureException in case of undeclared modules, if {@link #failOnUndeclaredModules}
   */
  private void checkModules(File appFile) throws MojoExecutionException, MojoFailureException {
    Log log = getLog();
    String name = this.project.getArtifactId();
    String expression = String.format(EXTRACT_MODULES, appFile.getPath(), name, name);
    String moduleStr = eval(log, expression, getDependencies(this.targetLib));
    Set<String> appModules = new HashSet<String>(Arrays.asList(moduleStr.split(" ")));
    Set<String> modules = new HashSet<String>();
    for (File beam : getFilesRecursive(this.targetEbin, BEAM_SUFFIX)) {
      modules.add(beam.getName().replace(BEAM_SUFFIX, ""));
    }
    if (!modules.containsAll(appModules) || !appModules.containsAll(modules)) {
      Set<String> undeclared = new HashSet<String>(modules);
      undeclared.removeAll(appModules);
      log.warn("Undeclared modules: " + undeclared.toString());
      Set<String> unbacked = new HashSet<String>(appModules);
      unbacked.removeAll(modules);
      log.warn("Unbacked modules: " + unbacked.toString());
      if (this.failOnUndeclaredModules) {
        throw new MojoFailureException("Module mismatch found.");
      }
    }
  }

  /**
   * Checks the erlang application upgrade file for plausability.
   * 
   * @param appUpFile the erlang application upgrade file
   * @throws MojoExecutionException
   * @throws MojoFailureException in case of problems with the appUpFile
   */
  private void checkAppUp(File appUpFile) throws MojoExecutionException, MojoFailureException {
    Log log = getLog();
    String version = this.project.getVersion();
    String expression = String.format(CHECK_APPUP, appUpFile.getPath(), version, version);
    String result = eval(log, expression, getDependencies(this.targetLib));
    if (!"ok".equals(result)) {
      log.error("Issue with .appup file : " + result);
      throw new MojoFailureException("Invalid .appup file.");
    }
  }

  /**
   * Returns a {@link String} containing all registered names of this
   * application in valid erlang list representation. This is done by scanning
   * the attributes of the given modules for the {@code -registered(Names).}
   * annotation.
   * 
   * @param modules valid erlang list representation of modules to scan
   * @return a {@link String} containing all found registered names
   * @throws MojoExecutionException
   * @throws MojoFailureException
   * @see #getModules(File)
   */
  private String getRegisteredNames(String modules) throws MojoExecutionException, MojoFailureException {
    String expression = String.format(EXTRACT_ATTRIBUTE, "registered", modules);
    return eval(getLog(), expression, getDependencies(this.targetLib), this.targetEbin);
  }

  /**
   * Returns a {@link String} containing all source modules in the given
   * directory in valid erlang list representation.
   * 
   * @param directory to scan for sources
   * @return a {@link String} containing all found source modules
   */
  private static String getModules(File directory) {
    StringBuilder modules = new StringBuilder("[");
    List<File> sources = getFilesRecursive(directory, BEAM_SUFFIX);
    for (int i = 0; i < sources.size(); ++i) {
      if (i != 0) {
        modules.append(", ");
      }
      modules.append(sources.get(i).getName().replace(BEAM_SUFFIX, ""));
    }
    modules.append("]");
    return modules.toString();
  }
}
