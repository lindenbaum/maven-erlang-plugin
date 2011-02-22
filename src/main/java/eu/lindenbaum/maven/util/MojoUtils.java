package eu.lindenbaum.maven.util;

import static eu.lindenbaum.maven.util.FileUtils.getDirectoriesRecursive;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import eu.lindenbaum.maven.Properties;

import org.apache.maven.plugin.Mojo;

/**
 * Containing utilities related to maven-erlang-plugin specific {@link Mojo}s.
 * 
 * @author Olle Törnström <olle.toernstroem@lindenbaum.eu>
 * @author Sven Heyll <sven.heyll@gmail.com>
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 * @since 2.0.0
 */
public final class MojoUtils {
  /**
   * Returns whether there are newer files in a specific directory (recursive)
   * than a given reference file.
   * 
   * @param dir input directory to check
   * @param reference the file taken as reference time (modified)
   * @return true if there are .erl/.hrl files newer than the reference file
   */
  public static boolean newerFilesThan(File dir, File reference) {
    final long referenceTime = reference.lastModified();
    if (referenceTime > 0L) {
      List<File> sources = FileUtils.getFilesRecursive(dir, ErlConstants.ERL_SUFFIX);
      sources.addAll(FileUtils.getFilesRecursive(dir, ErlConstants.HRL_SUFFIX));
      for (File file : sources) {
        if (file.lastModified() > referenceTime) {
          return true;
        }
      }
      return false;
    }
    return true;
  }

  /**
   * Gathers a complete list of directories that may contain erlang header
   * (".hrl") files included in a standard compilation process.
   * 
   * @param p a the properties object containing some of the include
   *          directories.
   * @return a list of directories that must be considered when resolving erlang
   *         header files.
   */
  public static List<File> getIncludeDirectories(Properties p) {
    List<File> includes = new ArrayList<File>();
    includes.addAll(FileUtils.getDirectoriesRecursive(p.targetLib(), ErlConstants.HRL_SUFFIX));
    includes.add(p.include());
    includes.add(p.targetInclude());
    includes.add(p.src());
    return includes;
  }

  /**
   * Gathers a complete list of directories that may contain erlang header
   * (".hrl") files included in a test compilation process.
   * 
   * @param p a the properties object containing some of the include
   *          directories.
   * @return a list of directories that must be considered when resolving erlang
   *         header files.
   */
  public static List<File> getTestIncludeDirectories(Properties p) {
    List<File> includes = new ArrayList<File>();
    includes.addAll(getDirectoriesRecursive(p.targetLib(), ErlConstants.HRL_SUFFIX));
    includes.add(p.include());
    includes.add(p.test_src());
    includes.add(p.test_include());
    includes.add(p.targetInclude());
    includes.add(p.src());
    return includes;
  }
}
