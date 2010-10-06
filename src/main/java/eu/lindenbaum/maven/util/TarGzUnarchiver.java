package eu.lindenbaum.maven.util;

import static eu.lindenbaum.maven.util.ErlUtils.eval;

import java.io.File;
import java.io.IOException;

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugin.logging.Log;

/**
 * An unarchiver that can extract gzipped tar archives using the erlang
 * {@code erl_tar} module. This archiver will overwrite older files by default.
 * 
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 */
public final class TarGzUnarchiver {
  private final Log log;
  private final File destination;

  public TarGzUnarchiver(Log log) {
    this(log, new File("."));
  }

  public TarGzUnarchiver(Log log, File destination) {
    this.log = log;
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
      this.destination.mkdirs();
      if (this.destination.exists()) {
        if (this.destination.isDirectory()) {
          StringBuilder command = new StringBuilder();
          command.append("erl_tar:extract(\"");
          command.append(archive.getAbsolutePath());
          command.append("\", [compressed]).");
          try {
            eval(this.log, command.toString(), null, this.destination);
          }
          catch (MojoExecutionException e) {
            throw new IOException(e.getMessage(), e);
          }
          catch (MojoFailureException e) {
            throw new IOException(e.getMessage(), e);
          }
        }
        else {
          throw new IOException(this.destination.getAbsolutePath() + " is not a directory");
        }
      }
      else {
        throw new IOException("could not create " + this.destination.getAbsolutePath());
      }
    }
    else {
      throw new IOException(archive.getAbsolutePath() + " does not exist");
    }
  }
}
