package eu.lindenbaum.maven.util;

/**
 * Defines Erlang related constants.
 * 
 * @author Olivier Sambourg
 * @author Paul Guyot
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 */
public interface ErlConstants {
  /**
   * Name of the erlang interpreter binary.
   */
  public static final String ERL = "erl";

  /**
   * Suffix for erlang source files.
   */
  public static final String ERL_SUFFIX = ".erl";

  /**
   * Suffix for erlang header files.
   */
  public static final String HRL_SUFFIX = ".hrl";

  /**
   * Suffix for application resource files.
   */
  public static final String APP_SUFFIX = ".app";

  /**
   * Suffix for application upgrade files.
   */
  public static final String APPUP_SUFFIX = ".appup";

  /**
   * Suffix for erlang binary files.
   */
  public static final String BEAM_SUFFIX = ".beam";

  /**
   * Suffix for rel files.
   */
  public static final String REL_SUFFIX = ".rel";

  /**
   * Suffix for src files, like erl.src, start.src, etc.
   */
  public static final String SRC_SUFFIX = ".src";

  /**
   * Suffix for tar gzipped files.
   */
  public static final String TARGZ_SUFFIX = ".tar.gz";

  /**
   * Name of the edoc application overview file.
   */
  public static final String OVERVIEW_EDOC = "overview.edoc";

  /**
   * Name of the system configuration file.
   */
  public static final String SYS_CONFIG = "sys.config";

  /**
   * Name of the release upgrade file.
   */
  public static final String RELUP = "relup";

  /**
   * Name of the coverdata binary (coverdata) file.
   */
  public static final String COVERDATA_BIN = "coverdata.coverdata";

  /**
   * Name of the dialyzer ok file.
   */
  public static final String DIALYZER_OK = ".dialyzer.ok";
}
