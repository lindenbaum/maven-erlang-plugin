package eu.lindenbaum.maven.erlang;

import com.ericsson.otp.erlang.OtpErlangObject;

/**
 * Interface for classes representing erlang scripts.
 * 
 * @param T return value of the processed script.
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 */
public interface Script<T> {
  public static String NL = System.getProperty("line.separator");

  /**
   * Returns an evaluateable {@link String} containing the erlang script of the
   * implementing class.
   * 
   * @return An erlang script that can be evaluated.
   */
  public String get();

  /**
   * Converts the result term of the {@link Script} execution into another
   * representation.
   * 
   * @param result The return term of the {@link Script} execution.
   * @return A {@link Script} specific (converted) return value.
   */
  public T handle(OtpErlangObject result);
}
