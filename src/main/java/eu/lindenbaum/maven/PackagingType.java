package eu.lindenbaum.maven;

/**
 * Enum for the supported packaging types.
 * <ul>
 * <li>erlang-otp: packaging type for erlang/OTP applications (with maven
 * standard directory layout)</li>
 * <li>erlang-std: packaging type for erlang/OTP applications (with OTP standard
 * directory layout)</li>
 * <li>erlang-rel: packaging type for erlang/OTP releases</li>
 * </ul>
 * 
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 */
public enum PackagingType {
  /**
   * Packaging type for erlang/OTP applications with maven standard directory
   * layout:
   * 
   * <pre>
   *  BASE
   *    +-- src/changes (changes.xml)
   *    +-- src/main/erlang (*.app, *.appup, *.erl, private *.hrl)
   *    +-- src/main/include (*.hrl)
   *    +-- src/main/priv (*)
   *    +-- src/site (overview.edoc, site.xml)
   *    +-- src/site/apt (*.apt)
   *    +-- src/test/include (*.hrl)
   *    +-- src/test/erlang (*.erl)
   *    +-- src/test/priv (*)
   *    +-- target (build artifacts)
   *    +-- pom.xml
   *    +-- *.rel
   *    +-- *.relup
   * </pre>
   */
  ERLANG_OTP("erlang-otp"),
  /**
   * Packaging type for erlang/OTP applications with erlang/OTP standard
   * directory layout:
   * 
   * <pre>
   *  BASE
   *    +-- ebin (*.app, *.appup)
   *    +-- include (*.hrl)
   *    +-- priv (*)
   *    +-- src (*.erl, private *.hrl)
   *    +-- src/changes (changes.xml)
   *    +-- src/site (overview.edoc, site.xml)
   *    +-- src/site/apt (*.apt)
   *    +-- test_include (*.hrl)
   *    +-- test_priv (*)
   *    +-- test_src (*.erl)
   *    +-- target (build artifacts)
   *    +-- pom.xml
   * </pre>
   */
  ERLANG_STD("erlang-std"),
  /**
   * Packaging type for erlang/OTP releases with trivial directory layout:
   * 
   * <pre>
   *  BASE (*.rel, *.relup)
   *    +-- target (build artifacts)
   *    +-- pom.xml
   *    +-- *.rel
   *    +-- *.relup
   * </pre>
   */
  ERLANG_REL("erlang-rel");

  private final String id;

  /**
   * Constructs a packaging type object.
   * 
   * @param id declaring the packaging type in the project pom.
   */
  private PackagingType(String id) {
    this.id = id;
  }

  /**
   * Compares a specific id with this types id.
   * 
   * @param id to compare to this enums id
   * @return {@code true} if the ids match, {@code false} otherwise
   */
  public boolean isA(String id) {
    return this.id.equals(id);
  }

  /**
   * Returns a {@link PackagingType} for a packaging id. If the string does not
   * match a specific enum type an {@link IllegalArgumentException} is thrown.
   * 
   * @param id used to instantiate a {@link PackagingType}
   * @return the matching packaging type
   * @throws IllegalArgumentException if no packaging type matches
   */
  public static PackagingType fromString(String id) {
    if (ERLANG_OTP.id.equals(id)) {
      return ERLANG_OTP;
    }
    else if (ERLANG_STD.id.equals(id)) {
      return ERLANG_STD;
    }
    else if (ERLANG_REL.id.equals(id)) {
      return ERLANG_REL;
    }
    else {
      throw new IllegalArgumentException("unsupported packaging type " + id);
    }
  }
}
