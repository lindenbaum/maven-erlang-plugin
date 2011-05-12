package eu.lindenbaum.maven.erlang;

import java.io.File;
import java.util.List;

/**
 * Interface representing the result returned by the {@link BeamCompilerScript}.
 * 
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 */
public interface CompilerResult {
  /**
   * Returns a {@link List} of failed compilation units.
   * 
   * @return A {@link List} containing the failed compilation units.
   */
  public List<File> getFailed();

  /**
   * Returns a {@link List} of skipped compilation units.
   * 
   * @return A {@link List} containing the skipped compilation units.
   */
  public List<File> getSkipped();

  /**
   * Returns a {@link List} of compiled compilation units.
   * 
   * @return A {@link List} containing the successfully compiled compilation
   *         units.
   */
  public List<File> getCompiled();

  /**
   * Returns a {@link List} of compile error outputs.
   * 
   * @return A {@link List} containing the compile errors of the compilation
   *         process.
   */
  public List<String> getErrors();

  /**
   * Returns a {@link List} of compile warning outputs.
   * 
   * @return A {@link List} containing the compile warnings of the compilation
   *         process.
   */
  public List<String> getWarnings();
}
