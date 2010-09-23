package eu.lindenbaum.maven.util;

/**
 * Defines a class processing an {@link String} input.
 * 
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 */
public interface Processor {
  /**
   * Call when the processor should handle some input.
   * 
   * @param input ro process
   */
  public void handle(String input);
}
