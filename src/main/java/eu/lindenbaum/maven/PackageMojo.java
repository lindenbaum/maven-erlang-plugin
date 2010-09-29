package eu.lindenbaum.maven;

import static eu.lindenbaum.maven.util.ErlConstants.APPUP_SUFFIX;
import static eu.lindenbaum.maven.util.ErlConstants.APP_SUFFIX;
import static eu.lindenbaum.maven.util.ErlConstants.BEAM_SUFFIX;
import static eu.lindenbaum.maven.util.ErlConstants.SRC_SUFFIX;
import static eu.lindenbaum.maven.util.ErlConstants.TARGZ_SUFFIX;
import static eu.lindenbaum.maven.util.ErlUtils.eval;
import static eu.lindenbaum.maven.util.FileUtils.BEAM_FILTER;
import static eu.lindenbaum.maven.util.FileUtils.NULL_FILTER;
import static eu.lindenbaum.maven.util.FileUtils.SNMP_FILTER;
import static eu.lindenbaum.maven.util.FileUtils.SOURCE_FILTER;
import static eu.lindenbaum.maven.util.FileUtils.copyDirectory;
import static eu.lindenbaum.maven.util.FileUtils.getDependencies;
import static eu.lindenbaum.maven.util.FileUtils.getDirectoriesRecursive;
import static eu.lindenbaum.maven.util.FileUtils.removeDirectory;

import java.io.File;
import java.io.FileFilter;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
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
 * {@code resources} directories and non-erlang sources as well as application
 * resource and upgrade files (if any).
 * </p>
 * <p>
 * The application resource and upgrade files are also checked for plausability
 * by checking the application version against the project version and checking
 * the application modules against the found, compiled modules.
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

  /**
   * Setting this to {@code true} will break the build when the application file
   * does not contain all found modules.
   * 
   * @parameter default-value="true"
   */
  private boolean failOnUndeclaredModules;

  public void execute() throws MojoExecutionException, MojoFailureException {
    Log log = getLog();
    log.info("------------------------------------------------------------------------");
    log.info("PACKAGING PROJECT");

    File tmpDir = new File(this.target, this.project.getArtifactId() + "-" + this.project.getVersion());
    tmpDir.mkdirs();

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
    targetDir.mkdirs();
    int copied = copyDirectory(srcDir, targetDir, filter);
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
    for (File beam : Arrays.asList(this.targetEbin.listFiles(BEAM_FILTER))) {
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
}
