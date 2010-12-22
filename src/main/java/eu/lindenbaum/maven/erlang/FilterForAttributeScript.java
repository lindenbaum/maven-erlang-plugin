package eu.lindenbaum.maven.erlang;

import java.io.File;
import java.util.List;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;

import eu.lindenbaum.maven.util.ErlUtils;

/**
 * A {@link Script} that filters a list of modules for the specification of a
 * certain attribute.
 * 
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 * @author Olle Törnström <olle.toernstroem@lindenbaum.eu>
 */
public class FilterForAttributeScript implements Script<String> {
  private static final String script = //
  "    lists:flatten(" + //
      "    lists:foldl(" + //
      "      fun(Module, Acc) ->" + //
      "              A = Module:module_info(attributes)," + //
      "              case proplists:get_value(%s, A) of" + //
      "                  undefined -> Acc;" + //
      "                  _ -> [Module | Acc]" + //
      "              end" + //
      "      end, [], %s)).";

  private final List<File> modules;
  private final String attribute;

  /**
   * Filters a list of modules for a specific attribute.
   * 
   * @param modules to filter
   * @param attribute to look after
   */
  public FilterForAttributeScript(List<File> modules, String attribute) {
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
   * containing an erlang list of modules specifying a specific attribute.
   * 
   * @param result to convert
   * @return A list of modules, never {@code null}.
   */
  @Override
  public String handle(OtpErlangObject result) {
    OtpErlangList resultList = (OtpErlangList) result;
    StringBuilder filtered = new StringBuilder("[");
    for (int i = 0; i < resultList.arity(); ++i) {
      if (i != 0) {
        filtered.append(", ");
      }
      filtered.append("'" + ErlUtils.cast(resultList.elementAt(i)) + "'");
    }
    filtered.append("]");
    return filtered.toString();
  }
}
