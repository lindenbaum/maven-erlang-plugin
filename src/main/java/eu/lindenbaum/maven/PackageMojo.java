package eu.lindenbaum.maven;

import static eu.lindenbaum.maven.util.ErlConstants.APPUP_SUFFIX;
import static eu.lindenbaum.maven.util.ErlConstants.APP_SUFFIX;
import static eu.lindenbaum.maven.util.ErlConstants.BEAM_SUFFIX;
import static eu.lindenbaum.maven.util.ErlConstants.EBIN_DIRECTORY;
import static eu.lindenbaum.maven.util.ErlConstants.INCLUDE_DIRECTORY;
import static eu.lindenbaum.maven.util.ErlConstants.MIBS_DIRECTORY;
import static eu.lindenbaum.maven.util.ErlConstants.PRIV_DIRECTORY;
import static eu.lindenbaum.maven.util.ErlConstants.SRC_SUFFIX;
import static eu.lindenbaum.maven.util.ErlConstants.TARGZ_SUFFIX;
import static eu.lindenbaum.maven.util.ErlUtils.eval;
import static eu.lindenbaum.maven.util.FileUtils.APP_FILTER;
import static eu.lindenbaum.maven.util.FileUtils.NULL_FILTER;
import static eu.lindenbaum.maven.util.FileUtils.SNMP_FILTER;
import static eu.lindenbaum.maven.util.FileUtils.SOURCE_FILTER;
import static eu.lindenbaum.maven.util.FileUtils.copyDirectory;
import static eu.lindenbaum.maven.util.FileUtils.getDirectoriesNonRecursive;
import static eu.lindenbaum.maven.util.FileUtils.getFilesRecursive;
import static eu.lindenbaum.maven.util.FileUtils.removeDirectory;
import static eu.lindenbaum.maven.util.MavenUtils.SEPARATOR;
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
 * project pom into the {@code .app} file. This can be done by using one of the
 * supported variables into the application resource files. Below is a list of
 * supported variables and their substitutions:
 * </p>
 * <ul>
 * <li><code>${ARTIFACT}</code>: the projects artifact id (atom)</li>
 * <li><code>${DESCRIPTION}</code>: the projects description (string)</li>
 * <li><code>${ID}</code>: the projects id (string)</li>
 * <li><code>${VERSION}</code>: the projects version (string)</li>
 * <li><code>${MODULES}</code>: all compiled {@code .beam} files found in the
 * target ebin folder (list)</li>
 * <li><code>${REGISTERED}</code>: all registered names, based on the
 * {@code -registered(Names).} attribute retrieved from the compiled
 * {@code .beam} files (list)</li>
 * </ul>
 * <p>
 * In case there is no application resouce file specified the {@link Mojo} will
 * generate a default {@code .app} file which looks like this:
 * </p>
 * 
 * <pre>
 * {application, ${ARTIFACT},
 *   [{description, ${DESCRIPTION}},
 *    {id, ${ID}},
 *    {vsn, ${VERSION}},
 *    {modules, ${MODULES}},
 *    {maxT, infinity},
 *    {registered, ${REGISTERED}},
 *    {included_applications, []},
 *    {applications, []},
 *    {env, []},
 *    {mod, undefined},
 *    {start_phases, []}]}.
 * </pre>
 * <p>
 * The resulting application resource file as well as the application upgrade
 * file will be checked for plausability regardless if generated or not. This is
 * done by checking the application version against the project version,
 * checking the application modules against the found compiled modules as well
 * as checking the application's start module.
 * </p>
 * 
 * @goal package
 * @phase package
 * @author Olivier Sambourg
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 * @author Olle Törnström <olle.toernstroem@lindenbaum.eu>
 */
public final class PackageMojo extends AbstractErlangMojo {
  private static final String EXTRACT_VERSION = //
  " " + "{ok, [{application, \'%s\', Properties}]} = file:consult(\"%s\"), " //
      + "{value, {vsn, Version}} = lists:keysearch(vsn, 1, Properties), " //
      + "io:format(Version), io:nl().";

  private static final String EXTRACT_MODULES = //
  " " + "{ok, [{application, \'%s\', Properties}]} = file:consult(\"%s\"), " //
      + "{value, {modules, M}} = lists:keysearch(modules, 1, Properties), " //
      + "Mods = lists:foldl(fun(Mod, Acc) -> Acc ++ io_lib:format(\"~s \", [Mod]) end, \"\", M), " //
      + "io:format(Mods), io:nl().";

  private static final String EXTRACT_START_MODULE = //
  " " + "{ok, [{application, \'%s\', Properties}]} = file:consult(\"%s\"), " //
      + "case proplists:get_value(mod, Properties) of" //
      + "  undefined -> ok;" //
      + "  {Module, _} -> io:format(\"~p\", [Module]), io:nl()" //
      + "end.";

  private static final String CHECK_APPUP = //
  " " + "{ok, [{\"%s\", UpFrom, DownTo}]} = file:consult(\"%s\"), " //
      + "lists:foreach(fun({_, Instructions}) -> " //
      + "  lists:foreach(fun(E) -> true = is_tuple(E) end," //
      + "Instructions) end, UpFrom ++ DownTo)," //
      + "io:format(\"ok\"), io:nl().";

  private static final String EXTRACT_ATTRIBUTE = //
  " " + "Attr = lists:foldl(fun(Module, Acc) ->" //
      + "A = Module:module_info(attributes)," //
      + "case proplists:get_value(\'%s\', A) of" //
      + "  undefined -> Acc;" + "  Registered -> [Registered | Acc]" //
      + "end end, [], %s)," //
      + "io:format(\"~p\", [lists:flatten(Attr)]), io:nl().";

  private static final String DEFAULT_APP = //
  "{application, ${ARTIFACT},\n" //
      + "  [{description,  ${DESCRIPTION}},\n" //
      + "   {id, ${ID}},\n" //
      + "   {vsn, ${VERSION}},\n" //
      + "   {modules, ${MODULES}},\n" //
      + "   {maxT, infinity},\n" //
      + "   {registered, ${REGISTERED}},\n" //
      + "   {included_applications, []},\n" //
      + "   {applications, []},\n" //
      + "   {env, []},\n" //
      + "   {mod, undefined},\n" //
      + "   {start_phases, []}]}.\n";

  /**
   * Setting this to {@code true} will break the build when the application file
   * does not contain all found modules.
   * 
   * @parameter default-value="true"
   */
  private boolean failOnUndeclaredModules;

  /**
   * Whether to delete the temporary packaging dir or not. Generally useful for
   * debugging. Setting this to {@code false} will preserve the temp dir.
   * 
   * @parameter default-value="true"
   */
  private boolean removeTempDir;

  @Override
  public void execute() throws MojoExecutionException, MojoFailureException {
    Log log = getLog();
    log.info(SEPARATOR);
    log.info("PACKAGING PROJECT");

    File tmpDir = new File(this.target, this.project.getArtifactId() + "-" + this.project.getVersion());
    tmpDir.mkdirs();

    // prepare and copy .app and .appup files
    String modules = getModules(this.targetEbin);
    Map<String, String> replacements = new HashMap<String, String>();
    replacements.put("${ARTIFACT}", "\'" + this.project.getArtifactId() + "\'");
    replacements.put("${DESCRIPTION}", "\"" + this.project.getDescription() + "\"");
    replacements.put("${ID}", "\"" + this.project.getId() + "\"");
    replacements.put("${VERSION}", "\"" + this.project.getVersion() + "\"");
    replacements.put("${MODULES}", modules);
    replacements.put("${REGISTERED}", getRegisteredNames(modules));

    File srcAppFile = new File(this.srcMainErlang, this.project.getArtifactId() + APP_SUFFIX);
    boolean srcAppFileExisted = srcAppFile.exists();
    if (!srcAppFileExisted) {
      try {
        fileWrite(srcAppFile.getPath(), "UTF-8", DEFAULT_APP);
      }
      catch (IOException e) {
        log.info("Could not find .app file, also failed to create default .app file, ignoring ...");
      }
    }
    copy(this.srcMainErlang, this.targetEbin, APP_FILTER, "application", replacements);
    if (!srcAppFileExisted) {
      srcAppFile.delete();
    }

    // package all otp standard resources
    copy(this.srcMainErlang, new File(tmpDir, "src"), SOURCE_FILTER, "source");
    copy(this.targetEbin, new File(tmpDir, EBIN_DIRECTORY), NULL_FILTER, "binary", null);
    copy(this.targetInclude, new File(tmpDir, INCLUDE_DIRECTORY), SOURCE_FILTER, "include");
    copy(this.srcMainInclude, new File(tmpDir, INCLUDE_DIRECTORY), SOURCE_FILTER, "include");
    copy(this.targetPriv, new File(tmpDir, PRIV_DIRECTORY), NULL_FILTER, "private");
    copy(this.srcMainErlang, new File(tmpDir, MIBS_DIRECTORY), SNMP_FILTER, "SNMP");
    copy(this.targetMibs, new File(tmpDir, MIBS_DIRECTORY), SNMP_FILTER, "SNMP");

    // package non erlang source folders, e.g. c, java, ... into c_src, java_src, ...
    FileFilter filter = new FileFilter() {
      @Override
      public boolean accept(File dir) {
        return dir.isDirectory() //
               && !dir.equals(PackageMojo.this.srcMainErlang) //
               && !dir.equals(PackageMojo.this.srcMainInclude) //
               && !dir.equals(PackageMojo.this.srcMainPriv) //
               && !dir.equals(PackageMojo.this.srcMainResources);
      }
    };
    for (File source : getDirectoriesNonRecursive(this.srcMain, filter)) {
      copy(source, new File(tmpDir, source.getName() + SRC_SUFFIX), NULL_FILTER, "non-erlang source");
    }

    // perform checks on .app and .appup files (if any)
    File appFile = new File(this.targetEbin, this.project.getArtifactId() + APP_SUFFIX);
    if (appFile.exists()) {
      checkVersion(appFile);
      checkModules(appFile);
      checkStartModule(appFile);

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
    log.info(SEPARATOR);

    if (this.removeTempDir) {
      getLog().info("Removing packaging temp directory " + tmpDir);
      removeDirectory(tmpDir);
    }
  }

  /**
   * Copy the content of a directory to another one applying filtering and
   * logging. If the target directory does not exist it will be created.
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
   * Copy the content of a directory to another one applying filtering and
   * logging. If the target directory does not exist it will be created. If
   * there were no files to copy the target directory will be removed.
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
    final int copied;
    if (r == null) {
      copied = copyDirectory(srcDir, targetDir, filter);
    }
    else {
      copied = copyDirectory(srcDir, targetDir, filter, r);
    }
    if (copied == 0) {
      targetDir.delete();
    }
    getLog().info("Copied " + copied + " " + kind + " files from " + srcDir + " to " + targetDir);
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
    String appFileVersion = eval(log, String.format(EXTRACT_VERSION, name, appFile.getPath()));
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
   * @throws MojoFailureException in case of undeclared modules, if
   *           {@link #failOnUndeclaredModules}
   */
  private void checkModules(File appFile) throws MojoExecutionException, MojoFailureException {
    Log log = getLog();
    String name = this.project.getArtifactId();
    String moduleStr = eval(log, String.format(EXTRACT_MODULES, name, appFile.getPath()));
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
   * Checks whether the start module of the project is correcty configured, by
   * means whether the module exists and whether it implements the
   * {@code application} behaviour.
   * 
   * @param appFile the erlang application upgrade file
   * @throws MojoExecutionException
   * @throws MojoFailureException in case of configuration problems with the
   *           start module
   */
  private void checkStartModule(File appFile) throws MojoExecutionException, MojoFailureException {
    Log log = getLog();
    String name = this.project.getArtifactId();
    String module = eval(log, String.format(EXTRACT_START_MODULE, name, appFile.getPath()));
    if (module != null && !module.isEmpty()) {
      File startModuleFile = new File(this.targetEbin, module + BEAM_SUFFIX);
      if (!startModuleFile.isFile()) {
        throw new MojoFailureException("Configured start module \'" + module + "\' does not exist.");
      }
      String expression = String.format(EXTRACT_ATTRIBUTE, "behaviour", "[" + module + "]");
      String behaviour = eval(log, expression, null, this.targetEbin);
      if (!behaviour.contains("application")) {
        throw new MojoFailureException("Configured start module \'" + module
                                       + "\' does not implement the application behaviour");
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
    String result = eval(log, String.format(CHECK_APPUP, version, appUpFile.getPath()));
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
    return eval(getLog(), expression, null, this.targetEbin);
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
      modules.append("\'" + sources.get(i).getName().replace(BEAM_SUFFIX, "") + "\'");
    }
    modules.append("]");
    return modules.toString();
  }
}
