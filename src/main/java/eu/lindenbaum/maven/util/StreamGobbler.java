package eu.lindenbaum.maven.util;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;

/**
 * Reads all input from an {@link InputStream} saving it into a (line based)
 * buffer.
 * 
 * @author Olivier Sambourg
 * @author Paul Guyot
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 */
public final class StreamGobbler implements Runnable {
  private final InputStream inputStream;
  private final Processor processor;

  public StreamGobbler(InputStream inputStream, Processor processor) {
    this.inputStream = inputStream;
    this.processor = processor;
  }

  @Override
  public void run() {
    try {
      InputStreamReader streamReader = new InputStreamReader(this.inputStream);
      BufferedReader reader = new BufferedReader(streamReader);
      String line;
      while ((line = reader.readLine()) != null) {
        if (!"".equals(line)) {
          this.processor.handle(line);
        }
      }
    }
    catch (IOException e) {
      e.printStackTrace();
    }
  }
}
