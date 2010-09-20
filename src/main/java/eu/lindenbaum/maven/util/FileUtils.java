package eu.lindenbaum.maven.util;

import java.io.File;
import java.io.FileFilter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * Containing utilities related to file handling.
 * 
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 */
public final class FileUtils {
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
}
