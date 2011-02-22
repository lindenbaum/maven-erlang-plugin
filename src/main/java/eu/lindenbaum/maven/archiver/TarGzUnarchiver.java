package eu.lindenbaum.maven.archiver;

import java.io.File;
import java.io.IOException;

import eu.lindenbaum.maven.erlang.MavenSelf;
import eu.lindenbaum.maven.erlang.Script;
import eu.lindenbaum.maven.util.FileUtils;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

import org.apache.maven.plugin.MojoExecutionException;

/**
 * An unarchiver that can extract gzipped tar archives using the erlang
 * {@code erl_tar} module. This archiver will overwrite older files by default.
 * 
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 */
public final class TarGzUnarchiver {
  private final String peer;
  private final String cookie;
  private final File destination;

  public TarGzUnarchiver(String peer, String cookie) {
    this(peer, cookie, new File("."));
  }

  public TarGzUnarchiver(String peer, String cookie, File destination) {
    this.peer = peer;
    this.cookie = cookie;
    this.destination = destination;
  }

  /**
   * Returns this unarchivers destination directory. No matter if existing or
   * not.
   * 
   * @return this unarchivers destination directory
   */
  public File getDestination() {
    return this.destination;
  }

  /**
   * Extracts a given archive into the configured directory.
   * 
   * @param archive to extract
   * @throws IOException in case the archive does not exist or the destination
   *           directory cannot be created or is not a directory or
   *           {@code erl_tar} ends with errors.
   */
  public void extract(File archive) throws IOException {
    if (archive.isFile()) {
      try {
        FileUtils.ensureDirectory(this.destination);
      }
      catch (MojoExecutionException e) {
        throw new IOException("failed to create destination directory " + this.destination);
      }
      if (this.destination.exists()) {
        if (this.destination.isDirectory()) {
          Script<String> script = new TarGzUnarchiverScript(archive, this.destination);
          try {
            String error = MavenSelf.get(this.cookie).exec(this.peer, script);
            if (error != null) {
              throw new IOException("failed to extract archive: " + error);
            }
          }
          catch (MojoExecutionException e) {
            throw new IOException(e.getMessage(), e);
          }
        }
        else {
          throw new IOException(this.destination.toString() + " is not a directory");
        }
      }
      else {
        throw new IOException("could not create " + this.destination);
      }
    }
    else {
      throw new IOException(archive.toString() + " does not exist");
    }
  }

  private static final class TarGzUnarchiverScript implements Script<String> {
    private static final String script = "erl_tar:extract(\"%s\", [compressed, {cwd, \"%s\"}]).";

    private final File archive;
    private final File destination;

    TarGzUnarchiverScript(File archive, File destination) {
      this.archive = archive;
      this.destination = destination;
    }

    @Override
    public String get() {
      String archivePath = this.archive.getAbsolutePath();
      String destinationPath = this.destination.getAbsolutePath();
      return String.format(script, archivePath, destinationPath);
    }

    /**
     * Converts the {@link Script} result into an error message, if any.
     * 
     * @param result The term returned by the script execution.
     * @return An error reason as returned by the {@link Script} execution,
     *         {@code null} on success.
     */
    @Override
    public String handle(OtpErlangObject result) {
      if (!"ok".equals(result.toString())) {
        OtpErlangTuple errorInfo = (OtpErlangTuple) result;
        OtpErlangTuple nameReason = (OtpErlangTuple) errorInfo.elementAt(1);
        return nameReason.elementAt(1).toString();
      }
      return null;
    }
  }
}
