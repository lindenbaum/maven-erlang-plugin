package eu.lindenbaum.maven.util;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;

/**
 * Reads all input from an {@link InputStream} saving it into a (line based) buffer.
 */
public final class StreamGobbler implements Runnable {
  private final InputStream inputStream;
  private final VoidProcedure<String> outputProcessor;

  public StreamGobbler(InputStream inputStream, VoidProcedure<String> outputProcessor) {
    this.inputStream = inputStream;
    this.outputProcessor = outputProcessor;
  }

  @Override
  public void run() {
    try {
      InputStreamReader streamReader = new InputStreamReader(this.inputStream);
      BufferedReader reader = new BufferedReader(streamReader);
      String line;
      while ((line = reader.readLine()) != null) {
        if (!"".equals(line)) {
          this.outputProcessor.apply(line);
        }
      }
    }
    catch (IOException e) {
      e.printStackTrace();
    }
  }
}
