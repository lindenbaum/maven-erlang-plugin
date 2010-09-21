package eu.lindenbaum.maven.util;

import java.io.File;
import java.io.FileFilter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.regex.Pattern;

import org.codehaus.plexus.util.SelectorUtils;

/**
 * Containing utilities related to file handling.
 * 
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 */
public final class FileUtils {
  static final String APP_REGEX = ".*" + File.separator + "(\\w+)-([\\d\\.]+)" + File.separator + "ebin$";
  static final Pattern APP_PATTERN = Pattern.compile(APP_REGEX);

  /**
   * Filename filter to filter source files (.erl & .hrl). Directories are always accepted.
   */
  public static final FileFilter SOURCE_FILTER = new FileFilter() {
    @Override
    public boolean accept(File pathname) {
      if (pathname.isFile()) {
        String name = pathname.getName();
        return name.endsWith(ErlConstants.HRL_SUFFIX) || name.endsWith(ErlConstants.ERL_SUFFIX);
      }
      else {
        return true;
      }
    }
  };

  /**
   * a {@link FileFilter} accepting all input
   */
  public static FileFilter NULL_FILTER = new FileFilter() {
    @Override
    public boolean accept(File pathname) {
      return true;
    }
  };

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
   * Get a {@link List} of directories matching the given filter.
   * 
   * @param root directory to start recursion from
   * @param filter used to filter directories
   * @return a {@link List} of found directories
   */
  public static List<File> getDirectoriesRecursive(File root, final FileFilter filter) {
    final List<File> files = new ArrayList<File>();
    if (root.isDirectory()) {
      files.addAll(Arrays.asList(root.listFiles(new FileFilter() {
        @Override
        public boolean accept(File child) {
          if (child.isDirectory()) {
            files.addAll(getDirectoriesRecursive(child, filter));
            return filter.accept(child);
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
   * Returns a list of all found filterend (sub) files and directories. In case a sub directory is excluded
   * all of its sub files are also excluded. By default patterns from
   * {@link org.codehaus.plexus.util.FileUtils#getDefaultExcludes()} will always be excluded.
   * 
   * @param root directory to start recursion from
   * @param filter used to filter the found files and directories
   * @return a {@link List} of found files and directories
   */
  public static List<File> getFilesAndDirectoriesRecursive(File root, final FileFilter filter) {
    final List<File> files = new ArrayList<File>();
    if (root.isDirectory()) {
      final String[] excludes = org.codehaus.plexus.util.FileUtils.getDefaultExcludes();
      files.addAll(Arrays.asList(root.listFiles(new FileFilter() {
        @Override
        public boolean accept(File child) {
          boolean accept = filter.accept(child);
          for (String exclude : excludes) {
            if (SelectorUtils.match(exclude, child.getAbsolutePath())) {
              accept = false;
              break;
            }
          }
          if (accept && child.isDirectory()) {
            files.addAll(getFilesAndDirectoriesRecursive(child, filter));
          }
          return accept;
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
          return "ebin".equals(dir.getName()) && APP_PATTERN.matcher(dir.getAbsolutePath()).matches();
        }
      };
      return getDirectoriesRecursive(root, filter);
    }
    return Collections.emptyList();
  }

  /**
   * Copies the contents of the source directory recursively into the destination directory. The source
   * directory must exist. All missing directories including the destination folder will be created if
   * necessary, already existing files will be overwritten.
   * 
   * @param from the source directory to copy from
   * @param to the destination directory to copy to
   * @param filter additional filter to apply before copying
   * @return the number of copied <b>files</b>
   * @see #getFilesAndDirectoriesRecursive(File, FileFilter)
   * @throws IOException
   */
  public static int copyDirectory(File from, File to, FileFilter filter) throws IOException {
    int copied = 0;
    if (from.exists() && from.isDirectory()) {
      List<File> toCopy = getFilesAndDirectoriesRecursive(from, filter);
      for (File src : toCopy) {
        File dest = new File(to, src.getAbsolutePath().replace(from.getAbsolutePath(), ""));
        if (src.isDirectory()) {
          dest.mkdirs();
        }
        else {
          org.codehaus.plexus.util.FileUtils.copyFile(src, dest);
          copied++;
        }
      }
    }
    return copied;
  }

  /**
   * Copies the contents of the source directory recursively into the destination directory. The source
   * directory must exist. All missing directories including the destination folder will be created if
   * necessary, already existing files will be overwritten. The given replacements will be applied to all
   * found files. It is assumed that all files are {@code UTF-8} encoded.
   * 
   * @param from the source directory to copy from
   * @param to the destination directory to copy to
   * @param filter additional filter to apply before copying
   * @param replacements a {@link Map} of {@link String} patterns to be replaced
   * @return the number of copied <b>files</b>
   * @see #getFilesAndDirectoriesRecursive(File, FileFilter)
   * @throws IOException
   */
  public static int copyDirectory(File from, File to, FileFilter filter, Map<String, String> replacements) throws IOException {
    int copied = 0;
    if (from.exists() && from.isDirectory()) {
      List<File> toCopy = getFilesAndDirectoriesRecursive(from, filter);
      for (File src : toCopy) {
        File dest = new File(to, src.getAbsolutePath().replace(from.getAbsolutePath(), ""));
        if (src.isDirectory()) {
          dest.mkdirs();
        }
        else {
          String content = org.codehaus.plexus.util.FileUtils.fileRead(src, "UTF-8");
          for (Entry<String, String> replacement : replacements.entrySet()) {
            content = content.replace(replacement.getKey(), replacement.getValue());
          }
          org.codehaus.plexus.util.FileUtils.fileWrite(dest.getAbsolutePath(), "UTF-8", content);
          copied++;
        }
      }
    }
    return copied;
  }
}
