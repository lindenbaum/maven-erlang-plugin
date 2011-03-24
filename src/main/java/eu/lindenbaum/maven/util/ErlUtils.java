package eu.lindenbaum.maven.util;

import java.io.File;
import java.util.Arrays;
import java.util.Collection;

import eu.lindenbaum.maven.erlang.CheckAppResult;
import eu.lindenbaum.maven.mojo.app.ResourceGenerator;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangInt;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangUInt;

import org.apache.maven.artifact.Artifact;

/**
 * Containing utilities related to erlang code execution.
 * 
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 * @author Gregory Haskins <ghaskins@novell.com>
 */
public final class ErlUtils {
  /**
   * Converts an array into a string containing a valid erlang list.
   * 
   * @param array to convert, maybe {@code null}
   * @param p optional predicat whether to include a specific list element,
   *          maybe {@code null}
   * @param prefix to prepend to an entry
   * @param postfix to append to an entry
   * @return a string representing a valid erlang list
   */
  public static <T> String toList(T[] array, Predicate<T> p, String prefix, String postfix) {
    if (array != null) {
      return toList(Arrays.asList(array), p, prefix, postfix);
    }
    return "[]";
  }

  /**
   * Converts a {@link Collection} into a string containing a valid erlang list.
   * 
   * @param list to convert
   * @param p optional predicat whether to include a specific list element,
   *          maybe {@code null}
   * @param prefix to prepend to an entry
   * @param postfix to append to an entry
   * @return a string representing a valid erlang list
   */
  public static <T> String toList(Collection<T> list, Predicate<T> p, String prefix, String postfix) {
    StringBuilder result = new StringBuilder("[");
    int i = 0;
    for (T elem : list) {
      if (p == null || p.pred(elem)) {
        if (i != 0) {
          result.append(", ");
        }
        result.append(prefix);
        result.append(elem.toString());
        result.append(postfix);
        i++;
      }
    }
    result.append("]");
    return result.toString();
  }

  /**
   * Converts a {@link Collection} of files into a string containing a valid
   * erlang list. The files will be converted using
   * {@link File#getAbsolutePath()}. The files will be checked for {@code null}
   * and existence. The prefix and postfix {@link String}s will be
   * prepended/appended to every element of the list. This may be used to quote
   * the returned paths correctly as erlang strings.
   * 
   * @param list to convert
   * @param prefix to prepend to an entry
   * @param postfix to append to an entry
   * @return a string representing a valid erlang list
   */
  public static String toFileList(Collection<File> list, String prefix, String postfix) {
    StringBuilder result = new StringBuilder("[");
    int i = 0;
    for (File file : list) {
      if (file != null && file.exists()) {
        if (i != 0) {
          result.append(", ");
        }
        result.append(prefix);
        result.append(file.getAbsolutePath());
        result.append(postfix);
        i++;
      }
    }
    result.append("]");
    return result.toString();
  }

  /**
   * Converts a {@link Collection} of erlang source or beam files into a string
   * containing a valid erlang list of module names. The files will be checked
   * for {@code null} and existence. The prefix and postfix {@link String}s will
   * be prepended/appended to every element of the list. This may be used to
   * quote the module list correctly.
   * 
   * @param list to convert
   * @param prefix to prepend to an entry
   * @param postfix to append to an entry
   * @return a string representing a valid erlang list
   */
  public static String toModuleList(Collection<File> list, String prefix, String postfix) {
    StringBuilder result = new StringBuilder("[");
    int i = 0;
    for (File file : list) {
      if (file != null && file.exists()) {
        if (i != 0) {
          result.append(", ");
        }
        result.append(prefix);
        String module = file.getName().replace(ErlConstants.BEAM_SUFFIX, "");
        result.append(module.replace(ErlConstants.ERL_SUFFIX, ""));
        result.append(postfix);
        i++;
      }
    }
    result.append("]");
    return result.toString();
  }

  /**
   * Converts an {@link OtpErlangObject} into a {@link String} using the object
   * specific conversion function. If there is no specific string conversion
   * function available the default {@link OtpErlangObject#toString()} is used.
   * Empty {@link OtpErlangList}s will result in an empty {@link String}.
   * 
   * @param object to convert
   * @return a non-null, trimmed {@link String} object
   */
  public static String toString(OtpErlangObject object) {
    if (object instanceof OtpErlangString) {
      return ((OtpErlangString) object).stringValue().trim();
    }
    if (object instanceof OtpErlangAtom) {
      return ((OtpErlangAtom) object).atomValue().trim();
    }
    if (object instanceof OtpErlangList && ((OtpErlangList) object).arity() == 0) {
      return "";
    }
    return object.toString().trim();
  }

  /**
   * Converts an {@link OtpErlangInt} or an {@link OtpErlangUInt} into an
   * {@code int} using the object specific conversion function. If the
   * conversion can't be performed, or throws an error during conversion, the
   * value {@code 0} is always returned.
   * 
   * @param object to convert
   * @return the object {@code int} value or {@code 0} if unable to convert
   */
  public static int toInt(OtpErlangObject object) {
    try {
      if (object instanceof OtpErlangInt) {
        return ((OtpErlangInt) object).intValue();
      }
      if (object instanceof OtpErlangUInt) {
        return ((OtpErlangUInt) object).intValue();
      }
      if (object instanceof OtpErlangLong) {
        return ((OtpErlangLong) object).intValue();
      }
    }
    catch (OtpErlangRangeException e) {
      // Ok
    }
    return 0;
  }

  /**
   * Returns a comma separated string of application version tuples taken from
   * the given array of artifacts. Result string will look like
   * <code>{"app1", "version1"}, {"app2", "version2"}, ...</code>.
   * 
   * @param artifacts to convert into application version tuples
   * @return a non-{@code null} {@link String} object
   */
  public static String toApplicationTuples(Artifact... artifacts) {
    StringBuilder applications = new StringBuilder();
    int i = 0;
    for (Artifact artifact : artifacts) {
      if (i++ != 0) {
        applications.append(",\n  ");
      }
      applications.append("{\'");
      applications.append(artifact.getArtifactId());
      applications.append("\', \"");
      applications.append(artifact.getBaseVersion());
      applications.append("\"}");
    }
    return applications.toString();
  }

  /**
   * Returns a comma separated string of application version tuples taken from
   * the given array of {@link CheckAppResult}s. Result string will look like
   * <code>{"app1", "version1"}, {"app2", "version2"}, ...</code>.
   * 
   * @param applicationInfos to convert into application version tuples
   * @return a non-{@code null} {@link String} object
   */
  public static String toApplicationTuples(CheckAppResult... applicationInfos) {
    StringBuilder applications = new StringBuilder();
    int i = 0;
    for (CheckAppResult applicationInfo : applicationInfos) {
      if (i++ != 0) {
        applications.append(",\n  ");
      }
      applications.append("{\'");
      applications.append(applicationInfo.getName());
      applications.append("\', \"");
      applications.append(applicationInfo.getVersion());
      applications.append("\"}");
    }
    return applications.toString();
  }

  /**
   * Generates a simple comma separated list of applications derived from the
   * artifacts listed as dependencies. Intended to be used to fill in
   * ${APPLICATIONS} in {@link ResourceGenerator}.
   * 
   * @param artifacts to convert into an artifactId listing
   * @return a non-{@code null} {@link String} containing a valid erlang list
   */
  public static String toArtifactIdListing(Collection<Artifact> artifacts) {
    StringBuilder applications = new StringBuilder();
    int i = 0;
    for (Artifact artifact : artifacts) {
      if (i++ != 0) {
        applications.append(", ");
      }

      applications.append(artifact.getArtifactId());
    }
    return applications.toString();
  }
}
