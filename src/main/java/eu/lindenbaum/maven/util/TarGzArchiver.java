package eu.lindenbaum.maven.util;

import static eu.lindenbaum.maven.util.ErlUtils.eval;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugin.logging.Log;

/**
 * An archiver that can create gzipped tar archives using the erlang
 * {@code erl_tar} module.
 * 
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 */
public final class TarGzArchiver {
  private final Log log;
  private final File archive;
  private final Map<File, String> files = new HashMap<File, String>();

  public TarGzArchiver(Log log, File archive) {
    this.log = log;
    this.archive = archive;
  }

  /**
   * Returns this archivers archive file no matter already created or not.
   * 
   * @return this archivers archive file
   */
  public File getArchive() {
    return this.archive;
  }

  /**
   * Add a file/directory to the archive.
   * 
   * @param file to add to the archive
   * @throws IOException if the file does not exist
   */
  public void addFile(File file) throws IOException {
    addFile(file, file.getName());
  }

  /**
   * Add a file/directory to the archive.
   * 
   * @param file to add to the archive
   * @param archiveName name of the file in the archive
   * @throws IOException if the file does not exist
   */
  public void addFile(File file, String archiveName) throws IOException {
    if (file.exists()) {
      this.files.put(file, archiveName);
    }
    else {
      throw new IOException("cannot add non-existing file " + file);
    }
  }

  /**
   * Creates and writes the archive to {@link #archive}.
   * 
   * @throws IOException if there are no files to archive or {@code erl_tar}
   *           ends with erros
   */
  public void createArchive() throws IOException {
    if (this.files.isEmpty()) {
      throw new IOException("no files to package");
    }
    else {
      StringBuilder command = new StringBuilder();
      command.append("erl_tar:create(\"");
      command.append(this.archive.getAbsolutePath());
      command.append("\", [");
      for (Entry<File, String> file : this.files.entrySet()) {
        command.append("{\"");
        command.append(file.getValue());
        command.append("\",\"");
        command.append(file.getKey().getAbsolutePath());
        command.append("\"},");
      }
      command.deleteCharAt(command.length() - 1);
      command.append("], [compressed]).");

      try {
        eval(this.log, command.toString());
      }
      catch (MojoExecutionException e) {
        throw new IOException(e.getMessage(), e);
      }
      catch (MojoFailureException e) {
        throw new IOException(e.getMessage(), e);
      }
    }
  }
}
