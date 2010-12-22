package eu.lindenbaum.maven.archiver;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

import eu.lindenbaum.maven.erlang.MavenSelf;
import eu.lindenbaum.maven.erlang.Script;

import org.apache.maven.plugin.MojoExecutionException;

/**
 * An archiver that can create gzipped tar archives using the erlang
 * {@code erl_tar} module.
 * 
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 */
public final class TarGzArchiver {
  private final String peer;
  private final String cookie;
  private final File archive;
  private final Map<File, String> files = new HashMap<File, String>();

  public TarGzArchiver(String peer, String cookie, File archive) {
    this.peer = peer;
    this.cookie = cookie;
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
      Script<String> script = new TarGzArchiverScript(this.archive, this.files);
      try {
        String error = MavenSelf.get(this.cookie).exec(this.peer, script);
        if (error != null) {
          throw new IOException("failed to create archive: " + error);
        }
      }
      catch (MojoExecutionException e) {
        throw new IOException(e.getMessage(), e);
      }
    }
  }

  private static final class TarGzArchiverScript implements Script<String> {
    private static final String script = "erl_tar:create(\"%s\", %s, [compressed]).";

    private final File archive;
    private final Map<File, String> files;

    TarGzArchiverScript(File archive, Map<File, String> files) {
      this.archive = archive;
      this.files = files;
    }

    @Override
    public String get() {
      String archivePath = this.archive.getAbsolutePath();
      return String.format(script, archivePath, getFiles(this.files));
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

    /**
     * Returns a list of the files to archive. It is assumed that there's more
     * than zero files to archive.
     * 
     * @param files mapping of files to package
     * @return a string containing an erlang list of file mappings
     */
    private static String getFiles(Map<File, String> files) {
      StringBuilder fileList = new StringBuilder("[");
      for (Entry<File, String> file : files.entrySet()) {
        fileList.append("{\"");
        fileList.append(file.getValue());
        fileList.append("\",\"");
        fileList.append(file.getKey().getAbsolutePath());
        fileList.append("\"},");
      }
      fileList.deleteCharAt(fileList.length() - 1);
      fileList.append("]");
      return fileList.toString();
    }
  }
}
