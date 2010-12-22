package eu.lindenbaum.maven.erlang;

import java.io.File;
import java.util.List;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;

import eu.lindenbaum.maven.util.ErlUtils;

/**
 * A {@link Script} that returns the found values for an attribute in a list of
 * modules.
 * 
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 * @author Olle Törnström <olle.toernstroem@lindenbaum.eu>
 */
public class GetAttributesScript implements Script<String> {
  private static final String script = //
  "    lists:flatten(" + //
      "    lists:foldl(" + //
      "      fun(Module, Acc) ->" + //
      "              A = Module:module_info(attributes)," + //
      "              case proplists:get_value(%s, A) of" + //
      "                  undefined -> Acc;" + //
      "                  Attr -> [Attr | Acc]" + //
      "              end" + //
      "      end, [], %s)).";

  private final List<File> modules;
  private final String attribute;

  /**
   * Returns the attribute values for a specific attribute.
   * 
   * @param modules to search for the attribute in
   * @param attribute to look after
   */
  public GetAttributesScript(List<File> modules, String attribute) {
    this.modules = modules;
    this.attribute = attribute;
  }

  @Override
  public String get() {
    String modules = ErlUtils.toModuleList(this.modules, "'", "'");
    return String.format(script, this.attribute, modules);
  }

  /**
   * Converts the result of the {@link Script} execution into a {@link String}
   * containing an erlang list of attribute values found. Attribute values are
   * expected to be atoms (or list of atoms).
   * 
   * @param result to convert
   * @return A list of modules, never {@code null}.
   */
  @Override
  public String handle(OtpErlangObject result) {
    OtpErlangList resultList = (OtpErlangList) result;
    StringBuilder attributes = new StringBuilder("[");
    for (int i = 0; i < resultList.arity(); ++i) {
      if (i != 0) {
        attributes.append(", ");
      }
      attributes.append("'" + ErlUtils.cast(resultList.elementAt(i)) + "'");
    }
    attributes.append("]");
    return attributes.toString();
  }
}
