package eu.lindenbaum.maven.mojo.rel;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.List;

import eu.lindenbaum.maven.ErlangMojo;
import eu.lindenbaum.maven.PackagingType;
import eu.lindenbaum.maven.Properties;
import eu.lindenbaum.maven.archiver.TarGzArchiver;
import eu.lindenbaum.maven.archiver.TarGzUnarchiver;
import eu.lindenbaum.maven.erlang.CreateRELEASESScript;
import eu.lindenbaum.maven.erlang.MavenSelf;
import eu.lindenbaum.maven.erlang.RuntimeInfo;
import eu.lindenbaum.maven.erlang.RuntimeInfoScript;
import eu.lindenbaum.maven.erlang.Script;
import eu.lindenbaum.maven.util.ErlConstants;
import eu.lindenbaum.maven.util.FileUtils;
import eu.lindenbaum.maven.util.MavenUtils;

import org.apache.maven.plugin.Mojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;

/**
 * <p>
 * This {@link Mojo} creates a target system release package {@code .tar.gz}
 * according to the official documentation. If there's no
 * <code>sys.config</code> file found a default empty one will be included. The
 * resulting target system depends on a correct root directory configuration.
 * The plug-in will change the scripts to check for the <code>${</code>
 * <i>ARTIFACTID</i><code>_TOP}</code> (upper case) environment variable. This
 * variable must be set by the user to guarantee a proper system behaviour.
 * Example: If artifact id is <code>release</code> the environment variable
 * <code>${RELEASE_TOP}</code> must be set.
 * </p>
 * <p>
 * Note: The resulting target system is highly system dependent since it
 * contains the erlang emulator (C code) from the backend nodes erlang
 * installation.
 * </p>
 * <p>
 * Note: This mojo is currently working but still in experimental stage.
 * </p>
 * <ul>
 * <li>FIXME binary names on windows may not be covered by this implementation</li>
 * </ul>
 * 
 * @goal target-system
 * @execute phase="package" lifecycle="targetSystem"
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 * @see <a
 *      href="http://www.erlang.org/doc/system_principles/create_target.html">http://www.erlang.org/doc/system_principles/create_target.html</a>
 */
public final class TargetSystemPackager extends ErlangMojo {
  @Override
  protected void execute(Log log, Properties p) throws MojoExecutionException {
    log.info(MavenUtils.SEPARATOR);
    log.info(" T A R G E T - S Y S T E M - P A C K A G E R");
    log.info(MavenUtils.SEPARATOR);

    PackagingType packagingType = p.packagingType();
    if (PackagingType.ERLANG_REL != packagingType) {
      log.info("Nothing to do for packaging " + packagingType + ".");
      return;
    }

    File tmp = new File(p.target(), "tmp");
    File tmpBin = new File(tmp, "bin");
    File tmpLog = new File(tmp, "log");
    FileUtils.ensureDirectories(tmp, tmpBin, tmpLog);

    {
      // write dummy file to guard to log directory
      File readme = new File(tmpLog, "README_NOT");
      String data = "README\n\nIf this directory does not exist 'start' will fail silently.\n";
      FileUtils.writeFile(readme, data);
    }

    String releaseName = p.project().getArtifactId();
    String releaseFileBaseName = releaseName + "-" + p.project().getVersion();

    // extract .tar.gz from systools:make_tar/2 to target/tmp
    File releaseTarGz = new File(p.target(), releaseFileBaseName + ErlConstants.TARGZ_SUFFIX);
    TarGzUnarchiver unarchiver = new TarGzUnarchiver(p.node(), p.cookie(), tmp);
    try {
      unarchiver.extract(releaseTarGz);
    }
    catch (IOException e) {
      throw new MojoExecutionException("Failed to extract " + releaseTarGz + ": " + e.getMessage());
    }

    RuntimeInfoScript infoScript = new RuntimeInfoScript();
    RuntimeInfo runtimeInfo = MavenSelf.get(p.cookie()).exec(p.node(), infoScript);
    File tmpLib = new File(tmp, "lib");
    File tmpReleases = new File(tmp, "releases");
    File tmpErts = new File(tmp, "erts-" + runtimeInfo.getVersion());
    File tmpErtsBin = new File(tmpErts, "bin");
    File tmpReleasesVersion = new File(tmpReleases, p.project().getVersion());

    // remove erl and start scripts from the erts-VERSION/bin directory
    FileUtils.removeFiles(new File(tmpErtsBin, "erl"), new File(tmpErtsBin, "start"));

    {
      // write start_erl.data file
      File startErlData = new File(tmpReleases, "start_erl.data");
      String data = runtimeInfo.getVersion() + " " + p.project().getVersion();
      FileUtils.writeFile(startErlData, data);
    }

    // copy epmd, run_erl, start_erl & start.boot to top level bin directory
    File epmd = new File(tmpErtsBin, "epmd");
    File runErl = new File(tmpErtsBin, "run_erl");
    File toErl = new File(tmpErtsBin, "to_erl");
    File startBoot = new File(tmpReleasesVersion, "start.boot");
    FileUtils.copyFiles(tmpBin, epmd, runErl, toErl, startBoot);

    // copy the erl.src, start.src and start_erl.src files to top level bin directory
    HashMap<String, String> replacements = new HashMap<String, String>();
    replacements.put("%EMU%", "beam");
    replacements.put("%FINAL_ROOTDIR%", "${" + releaseName.toUpperCase().replace("-", "_") + "_TOP}");
    FileUtils.copyDirectory(tmpErtsBin, tmpBin, FileUtils.SRC_FILTER, replacements);

    // remove .src extension from files previously copied & cleanup
    List<File> filesToRename = FileUtils.getFilesRecursive(tmpBin, ErlConstants.SRC_SUFFIX);
    FileUtils.removeFileNameSuffix(ErlConstants.SRC_SUFFIX, filesToRename.toArray(new File[0]));
    FileUtils.removeFilesRecursive(tmpErtsBin, ErlConstants.SRC_SUFFIX);

    // make files in bin directory executable
    for (File file : tmpBin.listFiles()) {
      file.setExecutable(true, false);
    }

    // create the initial RELEASES file
    String releaseFileName = releaseFileBaseName + ErlConstants.REL_SUFFIX;
    Script<String> script = new CreateRELEASESScript(tmp, new File(tmpReleases, releaseFileName));
    String result = MavenSelf.get(p.cookie()).exec(p.node(), script);
    if (!"ok".equals(result)) {
      throw new MojoExecutionException("Failed to create RELEASES file: " + result + ".");
    }

    // package target system
    String targetName = releaseFileBaseName + "-target-system" + ErlConstants.TARGZ_SUFFIX;
    File targetSystemTarGz = new File(p.target(), targetName);
    try {
      TarGzArchiver archiver = new TarGzArchiver(p.node(), p.cookie(), targetSystemTarGz);
      archiver.addFile(tmpBin);
      archiver.addFile(tmpLog);
      archiver.addFile(tmpErts);
      archiver.addFile(tmpLib);
      archiver.addFile(tmpReleases);
      archiver.createArchive();
    }
    catch (IOException e) {
      throw new MojoExecutionException("Failed to create " + targetSystemTarGz + ": " + e.getMessage());
    }

    // remove tmp directory
    FileUtils.removeDirectory(tmp);

    log.info("Successfully created target system package:");
    log.info(targetSystemTarGz.toString());
  }
}
