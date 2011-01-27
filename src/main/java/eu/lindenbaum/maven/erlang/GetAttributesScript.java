package eu.lindenbaum.maven.erlang;

import java.io.File;
import java.util.List;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;

import eu.lindenbaum.maven.util.ErlUtils;

/**
 * A {@link Script} that returns the found values for some attributes in a list
 * of modules.
 * 
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 * @author Olle Törnström <olle.toernstroem@lindenbaum.eu>
 */
public class GetAttributesScript implements Script<String> {
  private static final String script = //
  NL + "lists:flatten(" + NL + //
      "    lists:foldl(" + NL + //
      "      fun(Module, Acc) ->" + NL + //
      "              A = Module:module_info(attributes)," + NL + //
      "              lists:foldl(fun(Attr, InnerAcc) ->" + NL + //
      "                  case proplists:get_value(Attr, A) of" + NL + //
      "                      undefined -> InnerAcc;" + NL + //
      "                      Value -> [Value | InnerAcc]" + NL + //
      "                  end" + NL + //
      "              end, Acc, %s)" + NL + //
      "      end, [], %s))." + NL;

  private final List<File> modules;
  private final String[] attributes;

  /**
   * Returns the attribute values for attributes.
   * 
   * @param modules to search for the attribute in
   * @param attributes to look after
   */
  public GetAttributesScript(List<File> modules, String... attributes) {
    this.modules = modules;
    this.attributes = attributes;
  }

  @Override
  public String get() {
    String modules = ErlUtils.toModuleList(this.modules, "'", "'");
    String attributeList = ErlUtils.toList(this.attributes, null, "'", "'");
    return String.format(script, attributeList, modules);
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
      attributes.append("'" + ErlUtils.toString(resultList.elementAt(i)) + "'");
    }
    attributes.append("]");
    return attributes.toString();
  }
}
