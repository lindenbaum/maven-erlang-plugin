package eu.lindenbaum.maven.util;

import java.io.File;
import java.io.FileFilter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.regex.Pattern;

/**
 * Containing utilities related to file handling.
 * 
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 */
public final class FileUtils {
  static final String APP_REGEX = ".*" + File.separator + "(\\w+)-([\\d\\.]+)" + File.separator + "ebin$";
  static final Pattern APP_PATTERN = Pattern.compile(APP_REGEX);

  /**
   * Get a {@link List} of files matching the given file extension (excluding directories).
   * 
   * @param root directory to start recursion from
   * @param suffix file extension to match, can be e.g. eiter {@code ".erl"} or {@code "erl"}
   * @return a {@link List} of found files
   */
  public static List<File> getFilesRecursive(File root, final String suffix) {
    final List<File> files = new ArrayList<File>();
    if (root.isDirectory()) {
      files.addAll(Arrays.asList(root.listFiles(new FileFilter() {
        @Override
        public boolean accept(File child) {
          if (child.isDirectory()) {
            files.addAll(getFilesRecursive(child, suffix));
            return false;
          }
          else {
            return child.getName().endsWith(suffix);
          }
        }
      })));
    }
    return files;
  }

  /**
   * Get a {@link List} of directories matching the given name.
   * 
   * @param root directory to start recursion from
   * @param name matched against the directory name using {@link String#equals(Object)}
   * @return a {@link List} of found directories
   */
  public static List<File> getDirectoriesRecursive(File root, final String name) {
    final List<File> files = new ArrayList<File>();
    if (root.isDirectory()) {
      files.addAll(Arrays.asList(root.listFiles(new FileFilter() {
        @Override
        public boolean accept(File child) {
          if (child.isDirectory()) {
            files.addAll(getDirectoriesRecursive(child, name));
            return child.getName().equals(name);
          }
          else {
            return false;
          }
        }
      })));
    }
    return files;
  }

  /**
   * Return the list of the module-version/ebin/ paths in the given directory.
   * 
   * @param root directory to start the scan from
   * @return a list of matching {@link File}s
   */
  public static List<File> getDependencies(File root) {
    if (root != null && root.exists()) {
      FileFilter filter = new FileFilter() {
        @Override
        public boolean accept(File dir) {
          return APP_PATTERN.matcher(dir.getAbsolutePath()).matches();
        }
      };

      List<File> filtered = new ArrayList<File>();
      for (File dir : getDirectoriesRecursive(root, "ebin")) {
        if (filter.accept(dir)) {
          filtered.add(dir);
        }
      }
      return filtered;
    }
    return Collections.emptyList();
  }
}
