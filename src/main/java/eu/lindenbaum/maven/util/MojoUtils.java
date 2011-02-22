package eu.lindenbaum.maven.util;

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
   * Gathers a complete list of directories used as include directories in a
   * standard compilation process.
   * 
   * @param p the properties object containing some of the include directories
   * @return a non-{@code null} list of include directories
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
   * Gathers a complete list of directories used as include directories in a
   * test compilation process.
   * 
   * @param p the properties object containing some of the include directories
   * @return a non-{@code null} list of include directories
   */
  public static List<File> getTestIncludeDirectories(Properties p) {
    List<File> includes = new ArrayList<File>();
    includes.addAll(FileUtils.getDirectoriesRecursive(p.targetLib(), ErlConstants.HRL_SUFFIX));
    includes.add(p.include());
    includes.add(p.test_src());
    includes.add(p.test_include());
    includes.add(p.targetInclude());
    includes.add(p.src());
    return includes;
  }

  private static List<File> getDependencyCodePaths(Properties p) {
    return FileUtils.getDirectoriesRecursive(p.targetLib(), ErlConstants.BEAM_SUFFIX);
  }

  /**
   * Gathers a complete list of directories used as code paths in an
   * application's build process.
   * 
   * @param p the properties object containing the code path directories
   * @return a non-{@code null} list of code path directories
   */
  public static List<File> getApplicationCodePaths(Properties p) {
    List<File> codePaths = new ArrayList<File>(getDependencyCodePaths(p));
    codePaths.add(p.targetEbin());
    return codePaths;
  }

  /**
   * Gathers a complete list of directories used as code paths in an
   * application's test build process.
   * 
   * @param p the properties object containing the code path directories
   * @return a non-{@code null} list of code path directories
   */
  public static List<File> getApplicationTestCodePaths(Properties p) {
    List<File> codePaths = new ArrayList<File>(getDependencyCodePaths(p));
    codePaths.add(p.targetTestEbin());
    return codePaths;
  }

  /**
   * Gathers a complete list of directories used as code paths in a release
   * build process.
   * 
   * @param p the properties object containing the code path directories
   * @return a non-{@code null} list of code path directories
   */
  public static List<File> getReleaseCodePaths(Properties p) {
    List<File> codePaths = new ArrayList<File>(getDependencyCodePaths(p));
    codePaths.add(p.target());
    return codePaths;
  }
}
