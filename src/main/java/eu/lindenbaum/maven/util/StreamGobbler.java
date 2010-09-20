package eu.lindenbaum.maven.util;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;

import org.apache.maven.plugin.logging.Log;

/**
 * Reads all input from an {@link InputStream} saving it into a buffer.
 */
public final class StreamGobbler implements Runnable {
  private final InputStream inputStream;
  private final Log log;

  private final List<String> lineBuffer = new ArrayList<String>();

  public StreamGobbler(InputStream inputStream, Log log) {
    this.inputStream = inputStream;
    this.log = log;
  }

  /**
   * Returns the currently read lines.
   * 
   * @return a {@link List} containing all currently read lines
   */
  public List<String> getLines() {
    return this.lineBuffer;
  }

  @Override
  public void run() {
    try {
      InputStreamReader streamReader = new InputStreamReader(this.inputStream);
      BufferedReader reader = new BufferedReader(streamReader);
      String line;
      while ((line = reader.readLine()) != null) {
        if (!"".equals(line)) {
          this.lineBuffer.add(line);
          if (this.log.isDebugEnabled()) {
            this.log.debug(line);
          }
        }
      }
    }
    catch (IOException e) {
      this.log.debug("failed to read from stream " + this.inputStream, e);
    }
  }
}
