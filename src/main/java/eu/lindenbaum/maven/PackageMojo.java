package eu.lindenbaum.maven;

import static eu.lindenbaum.maven.util.ErlConstants.APPUP_SUFFIX;
import static eu.lindenbaum.maven.util.ErlConstants.APP_SUFFIX;
import static eu.lindenbaum.maven.util.ErlConstants.BEAM_SUFFIX;
import static eu.lindenbaum.maven.util.ErlUtils.eval;
import static eu.lindenbaum.maven.util.FileUtils.BEAM_FILTER;
import static eu.lindenbaum.maven.util.FileUtils.NULL_FILTER;
import static eu.lindenbaum.maven.util.FileUtils.SNMP_FILTER;
import static eu.lindenbaum.maven.util.FileUtils.SOURCE_FILTER;
import static eu.lindenbaum.maven.util.FileUtils.copyDirectory;
import static eu.lindenbaum.maven.util.FileUtils.getDependencies;
import static org.codehaus.plexus.util.FileUtils.deleteDirectory;

import java.io.File;
import java.io.FileFilter;
import java.io.IOException;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugin.logging.Log;
import org.apache.maven.project.MavenProject;
import org.codehaus.plexus.archiver.zip.ZipArchiver;

/**
 * Package the Erlang project.
 * 
 * @goal package
 * @phase package
 * @author Olivier Sambourg
 */
public final class PackageMojo extends AbstractMojo {
  /**
   * Command to extract the version from the .app file.
   */
  private static final String EXTRACT_VERSION = //
  "{ok, List} = file:consult(\"%s\"), " //
      + "{value, {application, %s, Properties}} = lists:keysearch(%s, 2, List), " //
      + "{value, {vsn, Version}} = lists:keysearch(vsn, 1, Properties), " //
      + "io:format(Version), io:nl().";

  /**
   * Command to extract the modules from the .app file.
   */
  private static final String EXTRACT_MODULES = //
  "{ok, List} = file:consult(\"%s\"), "
      + "{value, {application, %s, Properties}} = lists:keysearch(%s, 2, List), "
      + "{value, {modules, M}} = lists:keysearch(modules, 1, Properties), "
      + "Mods = lists:foldl(fun(Mod, Acc) -> Acc ++ io_lib:format(\"~s \", [Mod]) end, \"\", M), "
      + "io:format(\"~s\", [Mods]), io:nl().";

  /**
   * Command to check the .appup file.
   */
  private static final String CHECK_APPUP = //
  "{ok, List} = file:consult(\"%s\"), "
      + "{value, {\"%s\", UpFrom, DownTo}} = lists:keysearch(\"%s\", 1, List), "
      + "lists:foreach(fun({Version, ProcList}) -> "
      + "  lists:foreach(fun(ProcElement) -> true = is_tuple(ProcElement) end, ProcList) "
      + "end, UpFrom ++ DownTo)," + "io:format(\"ok\"), io:nl().";

  /**
   * {@link MavenProject} to process.
   * 
   * @parameter expression="${project}"
   * @required
   * @readonly
   */
  private MavenProject project;

  /**
   * Base directory for the build artifacts.
   * 
   * @parameter expression="${project.build.directory}"
   * @required
   * @readonly
   */
  private File buildOutput;

  /**
   * Final name.
   * 
   * @parameter expression="${project.build.finalName}"
   * @required
   * @readonly
   */
  private String buildName;

  /**
   * Directory where the erlang source files reside.
   * 
   * @parameter expression="${basedir}/src/main/erlang"
   * @required
   */
  private File srcMainErlang;

  /**
   * Directory where the erlang include files reside.
   * 
   * @parameter expression="${basedir}/src/main/include"
   * @required
   */
  private File srcMainInclude;

  /**
   * Directories where dependencies are unpacked.
   * 
   * @parameter expression="${project.build.directory}/lib/"
   * @required
   */
  private File libInput;

  /**
   * Directory where the beam files were created.
   * 
   * @parameter expression="${project.build.directory}/ebin"
   * @required
   */
  private File ebinInput;

  /**
   * Directory where the private files were created.
   * 
   * @parameter expression="${project.build.directory}/priv"
   * @required
   */
  private File privInput;

  /**
   * Directory where the SNMP specific files were created.
   * 
   * @parameter expression="${project.build.directory}/mibs"
   * @required
   */
  private File mibsInput;

  /**
   * Directory where resources reside.
   * 
   * @parameter expression="${basedir}/src/main/resources"
   * @required
   */
  private File srcMainResources;

  /**
   * Setting this to {@code true} will break the build when the application file does not contain all found
   * modules.
   * 
   * @parameter default-value="true"
   */
  private boolean failOnUndeclaredModules;

  /**
   * The Zip archiver.
   * 
   * @component role="org.codehaus.plexus.archiver.Archiver" roleHint="zip"
   * @required
   */
  private ZipArchiver zipArchiver;

  public void execute() throws MojoExecutionException, MojoFailureException {
    Log log = getLog();
    log.info("------------------------------------------------------------------------");
    log.info("PACKAGING PROJECT");

    File target = new File(this.buildOutput, this.buildName);
    target.mkdirs();

    try {
      copy(this.srcMainResources, target, NULL_FILTER, "resource");
      copy(this.srcMainErlang, new File(target, "src"), SOURCE_FILTER, "source");
      copy(this.ebinInput, new File(target, "ebin"), NULL_FILTER, "");
      copy(this.srcMainInclude, new File(target, "include"), SOURCE_FILTER, "include");
      copy(this.privInput, new File(target, "priv"), NULL_FILTER, "private");
      copy(this.mibsInput, new File(target, "mibs"), SNMP_FILTER, "SNMP");

      File appFile = new File(this.ebinInput, this.project.getArtifactId() + APP_SUFFIX);
      if (appFile.exists()) {
        checkVersion(appFile);
        checkModules(appFile);

        File appUpFile = new File(this.ebinInput, this.project.getArtifactId() + APPUP_SUFFIX);
        if (appUpFile.exists()) {
          checkAppUp(appUpFile);
        }
      }
      else {
        getLog().warn("No " + APP_SUFFIX + " file was found");
      }

      File toFile = new File(this.buildOutput, this.buildName + ".zip");
      try {
        this.zipArchiver.setIncludeEmptyDirs(false);
        this.zipArchiver.addDirectory(target, this.buildName + File.separator);
        this.zipArchiver.setDestFile(toFile);
        this.zipArchiver.createArchive();
        this.project.getArtifact().setFile(toFile);
      }
      catch (Exception e) {
        throw new MojoExecutionException(e.getMessage(), e);
      }
    }
    finally {
      try {
        log.info("------------------------------------------------------------------------");
        deleteDirectory(target);
      }
      catch (IOException e) {
      }
    }
  }

  /**
   * Copy the content of a directory to another one applying filtering and logging.
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
    String appFileVersion = eval(log, expression, getDependencies(this.libInput));
    if (!version.equals(appFileVersion)) {
      log.error("Version mismatch.");
      log.error("Project version is " + version + " while " + APP_SUFFIX + " version is " + appFileVersion);
      throw new MojoFailureException("Version mismatch " + version + " != " + appFileVersion);
    }
  }

  /**
   * Checks whether the modules to be packaged are declared in the erlang application file.
   * 
   * @param appFile the erlang application resource file
   * @throws MojoExecutionException
   * @throws MojoFailureException in case of undeclared modules, if {@link #failOnUndeclaredModules}
   */
  private void checkModules(File appFile) throws MojoExecutionException, MojoFailureException {
    Log log = getLog();
    String name = this.project.getArtifactId();
    String expression = String.format(EXTRACT_MODULES, appFile.getPath(), name, name);
    String moduleStr = eval(log, expression, getDependencies(this.libInput));
    Set<String> appModules = new HashSet<String>(Arrays.asList(moduleStr.split(" ")));
    Set<String> modules = new HashSet<String>();
    for (File beam : Arrays.asList(this.ebinInput.listFiles(BEAM_FILTER))) {
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
    String result = eval(log, expression, getDependencies(this.libInput));
    if (!"ok".equals(result)) {
      log.error("Issue with .appup file : " + result);
      throw new MojoFailureException("Invalid .appup file.");
    }
  }
}
