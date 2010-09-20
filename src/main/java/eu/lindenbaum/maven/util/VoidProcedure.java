package eu.lindenbaum.maven.util;

/**
 * Defines a procedure that takes one argument and returns a result.
 * 
 * @param <A> type of argument
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 */
public interface VoidProcedure<A> {
  public void apply(A arg);
}
