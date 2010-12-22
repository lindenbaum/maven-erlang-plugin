package eu.lindenbaum.maven.util;

import java.io.File;
import java.util.Arrays;
import java.util.Collection;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangInt;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangUInt;

/**
 * Containing utilities related to erlang code execution.
 * 
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
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
        result.append(file.getName()
                          .replace(ErlConstants.BEAM_SUFFIX, "")
                          .replace(ErlConstants.ERL_SUFFIX, ""));
        result.append(postfix);
        i++;
      }
    }
    result.append("]");
    return result.toString();
  }

  /**
   * Converts an {@link OtpErlangAtom}, {@link OtpErlangString} or an empty
   * {@link OtpErlangList} into a {@link String} using the object specific
   * conversion function. If the object is neither the {@link String}
   * {@code "undefined"} is returned.
   * 
   * @param object to convert
   * @return a non-null, trimmed {@link String} object
   */
  public static String cast(OtpErlangObject object) {
    if (object instanceof OtpErlangString) {
      return ((OtpErlangString) object).stringValue().trim();
    }
    if (object instanceof OtpErlangAtom) {
      return ((OtpErlangAtom) object).atomValue().trim();
    }
    if (object instanceof OtpErlangList && ((OtpErlangList) object).arity() == 0) {
      return "";
    }
    return "undefined";
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
}
