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

  public StreamGobbler(InputStream inputStream) {
    this(inputStream, null);
  }

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
      final InputStreamReader isr = new InputStreamReader(this.inputStream);
      final BufferedReader br = new BufferedReader(isr);
      String line;
      while ((line = br.readLine()) != null) {
        if (!"".equals(line)) {
          this.lineBuffer.add(line);

          if (this.log != null) {
            this.log.info(line);
          }
        }
      }
    }
    catch (IOException e) {
      e.printStackTrace();
    }
  }
}
