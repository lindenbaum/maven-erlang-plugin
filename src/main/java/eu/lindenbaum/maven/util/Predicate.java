package eu.lindenbaum.maven.util;

/**
 * An interface for predicates.
 * 
 * @param <T> type to evaluate the predicate on
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 */
public interface Predicate<T> {
  /**
   * Returns the result of the predicate for a specific object.
   * 
   * @param object to evaluate the predicate for
   * @return a predicate specific boolean value
   */
  public boolean pred(T object);
}
