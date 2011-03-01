package eu.lindenbaum.maven.erlang;

import java.io.File;
import java.util.List;

import eu.lindenbaum.maven.util.ErlUtils;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;

import org.apache.maven.plugin.MojoExecutionException;

/**
 * A {@link Script} that filters a list of modules for the specification of a
 * certain attribute.
 * 
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 * @author Olle Törnström <olle.toernstroem@lindenbaum.eu>
 */
public class FilterForAttributeScript extends AbstractScript<String> {
  private final List<File> modules;
  private final String attribute;

  /**
   * Filters a list of modules for a specific attribute.
   * 
   * @param modules to filter
   * @param attribute to look after
   */
  public FilterForAttributeScript(List<File> modules, String attribute) throws MojoExecutionException {
    super();
    this.modules = modules;
    this.attribute = attribute;
  }

  @Override
  public String get() {
    String modules = ErlUtils.toModuleList(this.modules, "'", "'");
    return String.format(this.script, this.attribute, modules);
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
      filtered.append("'" + ErlUtils.toString(resultList.elementAt(i)) + "'");
    }
    filtered.append("]");
    return filtered.toString();
  }
}
