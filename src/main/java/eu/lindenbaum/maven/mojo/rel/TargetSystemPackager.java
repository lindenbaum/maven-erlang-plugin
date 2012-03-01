package eu.lindenbaum.maven.mojo.rel;

import java.io.File;
import java.io.IOException;

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
import eu.lindenbaum.maven.util.MojoUtils;

import org.apache.maven.plugin.Mojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;

/**
 * <p>
 * This {@link Mojo} creates a target system release package {@code .tar.gz}
 * according to the official documentation. If there's no
 * <code>sys.config</code> file found a default empty one will be included.
 * </p>
 * <p>
 * The resulting target system can be started either directly or as a daemon
 * using the <code>bin/start</code> script. This script has to be provided with
 * the correct root directory of the target system. Custom arguments like node
 * name or cookie can be set by giving them as additional command line arguments
 * to <code>bin/start</code>.
 * </p>
 * <p>
 * Note: The resulting target system is highly system dependent since it
 * contains the erlang emulator (C code) from the backend nodes erlang
 * installation.
 * </p>
 * <ul>
 * <li>FIXME not supported on Microsoft Windows</li>
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
      throw new MojoExecutionException("Mojo does not support packaging type " + packagingType + ".");
    }

    if (MojoUtils.isWindows()) {
      throw new MojoExecutionException("Mojo is not supported on Microsoft Windows systems.");
    }

    File target = p.targetLayout().base();

    File tmp = new File(target, "tmp");
    File tmpBin = new File(tmp, "bin");
    File tmpLog = new File(tmp, "log");
    FileUtils.ensureDirectories(tmp, tmpBin, tmpLog);

    {
      // write dummy file to guard to log directory
      File readme = new File(tmpLog, "README_NOT");
      String data = "README\n\nIf this directory does not exist 'start' will fail silently.\n";
      FileUtils.writeFile(readme, data);
    }

    // extract .tar.gz from systools:make_tar/2 to target/tmp
    File releaseTarGz = p.targetLayout().projectArtifact();
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

    // remove erl and start scripts from the erts-VERSION/bin directory
    FileUtils.removeFiles(new File(tmpErtsBin, "erl"), new File(tmpErtsBin, "start"));

    {
      // write start_erl.data file
      File startErlData = new File(tmpReleases, "start_erl.data");
      String data = runtimeInfo.getVersion() + " " + p.project().getVersion();
      FileUtils.writeFile(startErlData, data);
    }

    // remove all .src file from the erst-VERSION/bin directory
    FileUtils.removeFilesRecursive(tmpErtsBin, ErlConstants.SRC_SUFFIX);

    // create the start and attach scripts in the top level bin directory
    File start = new File(tmpBin, "start");
    FileUtils.extractFileFromClassPath(getClass(), "", "start", start);
    File attach = new File(tmpBin, "attach");
    FileUtils.extractFileFromClassPath(getClass(), "", "attach", attach);

    // make files in bin directory executable
    for (File file : tmpBin.listFiles()) {
      file.setExecutable(true, false);
    }

    // create the initial RELEASES file
    String releaseFileName = p.targetLayout().relFile().getName();
    Script<String> script = new CreateRELEASESScript(tmp, new File(tmpReleases, releaseFileName));
    String result = MavenSelf.get(p.cookie()).exec(p.node(), script);
    if (!"ok".equals(result)) {
      throw new MojoExecutionException("Failed to create RELEASES file: " + result + ".");
    }

    // package target system
    String targetSystemSuffix = "-target-system" + ErlConstants.TARGZ_SUFFIX;
    String targetName = releaseTarGz.getName().replace(ErlConstants.TARGZ_SUFFIX, targetSystemSuffix);
    File targetSystemTarGz = new File(target, targetName);
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
