package eu.lindenbaum.maven.util;

import static eu.lindenbaum.maven.util.ErlConstants.APPUP_SUFFIX;
import static eu.lindenbaum.maven.util.ErlConstants.APP_SUFFIX;
import static eu.lindenbaum.maven.util.ErlConstants.BEAM_SUFFIX;
import static eu.lindenbaum.maven.util.ErlConstants.ERL_SUFFIX;
import static eu.lindenbaum.maven.util.ErlConstants.HRL_SUFFIX;
import static eu.lindenbaum.maven.util.ErlConstants.REL_SUFFIX;
import static eu.lindenbaum.maven.util.ErlConstants.SRC_SUFFIX;
import static org.codehaus.plexus.util.FileUtils.getDefaultExcludes;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileFilter;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import org.apache.maven.plugin.MojoExecutionException;
import org.codehaus.plexus.util.SelectorUtils;

/**
 * Containing utilities related to file handling.
 * 
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 */
public final class FileUtils {
  /**
   * Filename filter to filter source files (.erl & .hrl). Directories are
   * always accepted.
   */
  public static final FileFilter SOURCE_FILTER = getSuffixFilter(new String[]{ HRL_SUFFIX, ERL_SUFFIX });

  /**
   * Filename filter to filter compiled beam files (.beam). Directories are
   * always accepted.
   */
  public static final FileFilter BEAM_FILTER = getSuffixFilter(new String[]{ BEAM_SUFFIX });

  /**
   * Filename filter to filter app files (.app & .appup). Directories are always
   * accepted.
   */
  public static final FileFilter APP_FILTER = getSuffixFilter(new String[]{ APP_SUFFIX, APPUP_SUFFIX });

  /**
   * Filename filter to filter rel files (.rel). Directories are always
   * accepted.
   */
  public static final FileFilter REL_FILTER = getSuffixFilter(new String[]{ REL_SUFFIX });

  /**
   * Filename filter to filter .src files. Directories are always accepted.
   */
  public static final FileFilter SRC_FILTER = getSuffixFilter(new String[]{ SRC_SUFFIX });

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
   * a {@link Predicate} that checks a file for {@code null} and existance
   */
  public static Predicate<File> FILE_PRED = new Predicate<File>() {
    @Override
    public boolean pred(File file) {
      return file != null && file.exists();
    }
  };

  /**
   * Returns a {@link FileFilter} which only accepts <b>files</b> ending with
   * one of the given suffixes. The suffixes may be of the for {@code .java} or
   * {@code java}. Directories will always be accepted.
   * 
   * @param suffixes list of accepted suffixes
   * @return a new {@link FileFilter}
   */
  public static FileFilter getSuffixFilter(final String[] suffixes) {
    return new FileFilter() {
      @Override
      public boolean accept(File file) {
        if (file.isFile()) {
          for (String pattern : suffixes) {
            String name = file.getName();
            if (name.endsWith(pattern)) {
              return true;
            }
          }
          return false;
        }
        return true;
      }
    };
  }

  /**
   * Get a {@link List} of files matching the given file extension (excluding
   * directories). By default patterns from
   * {@link org.codehaus.plexus.util.FileUtils#getDefaultExcludes()} will always
   * be excluded.
   * 
   * @param root directory to start recursion from
   * @param suffix file extension to match, can be e.g. eiter {@code ".erl"} or
   *          {@code "erl"}
   * @return a {@link List} of found files
   */
  public static List<File> getFilesRecursive(File root, final String suffix) {
    final List<File> files = new ArrayList<File>();
    if (root.isDirectory()) {
      final String[] excludes = getDefaultExcludes();
      files.addAll(Arrays.asList(root.listFiles(new FileFilter() {
        @Override
        public boolean accept(File child) {
          if (child.isDirectory()) {
            for (String exclude : excludes) {
              if (SelectorUtils.match(exclude, child.getAbsolutePath())) {
                return false;
              }
            }
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
   * Returns the file object of a certain file contained in a given list. The
   * files name will be compared to {@link File#getName()}.
   * 
   * @param name file name to look for
   * @param files to examine for the corresponding file object
   * @return The {@link File} referring to the given name or {@code null} if no
   *         file with the file name could be found.
   */
  public static File getFile(String name, Collection<File> files) {
    for (File file : files) {
      if (name.equals(file.getName())) {
        return file;
      }
    }
    return null;
  }

  /**
   * Removes all files ending with a specific suffix recursively from a
   * directory. By default patterns from
   * {@link org.codehaus.plexus.util.FileUtils#getDefaultExcludes()} will always
   * be excluded.
   * 
   * @param root directory to remove specific files from
   * @param suffix file suffixes to remove
   * @return the number of removed files
   */
  public static int removeFilesRecursive(File root, String suffix) {
    List<File> files = getFilesRecursive(root, suffix);
    return removeFiles(files.toArray(new File[0]));
  }

  /**
   * Removes a directory recursively.
   * 
   * @param directory to remove
   */
  public static void removeDirectory(File directory) {
    try {
      org.codehaus.plexus.util.FileUtils.deleteDirectory(directory);
    }
    catch (IOException e) {
      // ignore
    }
  }

  /**
   * Removes an empty directory. Non-empty directories will not be deleted.
   * 
   * @param directory to remove
   */
  public static void removeEmptyDirectory(File directory) throws MojoExecutionException {
    if (directory.isDirectory()) {
      String[] list = directory.list();
      if (list != null && list.length == 0) {
        if (!directory.delete()) {
          throw new MojoExecutionException("Failed to delete empty directory " + directory + ".");
        }
      }
    }
  }

  /**
   * Removes the specific files from the file system. Returns the number of
   * files removed. Directories are skipped.
   * 
   * @param files to remove.
   * @return The number of files actually removed.
   */
  public static int removeFiles(File... files) {
    int removed = 0;
    for (File file : files) {
      if (file.isFile()) {
        if (file.delete()) {
          removed++;
        }
      }
    }
    return removed;
  }

  /**
   * Renames a list of files by replacing the given string suffix from the
   * current file name. Directories will be skipped.
   * 
   * @param suffix to remove
   * @param files to remove the filename suffix from
   * @throws MojoExecutionException in case renaming fails
   */
  public static void removeFileNameSuffix(String suffix, File... files) throws MojoExecutionException {
    for (File file : files) {
      if (file.isFile()) {
        String newFileName = file.getName().replace(suffix, "");
        File dest = new File(file.getParentFile(), newFileName);
        try {
          org.codehaus.plexus.util.FileUtils.rename(file, dest);
        }
        catch (IOException e) {
          throw new MojoExecutionException("Failed to rename " + file + ".");
        }
      }
    }
  }

  /**
   * Returns a list of all found filterend (sub) files and directories. In case
   * a sub directory is excluded all of its sub files are also excluded. By
   * default patterns from
   * {@link org.codehaus.plexus.util.FileUtils#getDefaultExcludes()} will always
   * be excluded.
   * 
   * @param root directory to start recursion from
   * @param filter used to filter the found files and directories
   * @return a {@link List} of found files and directories
   */
  public static List<File> getFilesAndDirectoriesRecursive(File root, final FileFilter filter) {
    final List<File> files = new ArrayList<File>();
    if (root.isDirectory()) {
      final String[] excludes = getDefaultExcludes();
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
   * Returns the list of sub directories containing files with the specified
   * file suffix. By default patterns from
   * {@link org.codehaus.plexus.util.FileUtils#getDefaultExcludes()} will always
   * be excluded.
   * 
   * @param root directory to start the scan from
   * @param suffix file suffix to search for
   * @return a non-{@code null} list of directories containg files with a
   *         certain suffix
   */
  public static List<File> getDirectoriesRecursive(File root, String suffix) {
    Set<File> result = new HashSet<File>();
    if (root != null && root.exists()) {
      for (File beam : getFilesRecursive(root, suffix)) {
        File parentFile = beam.getParentFile();
        if (parentFile != null) {
          result.add(parentFile);
        }
      }
    }
    return new ArrayList<File>(result);
  }

  /**
   * Returns a list of sub directories of a specific directory not matching a
   * list of given excludes. By default patterns from
   * {@link org.codehaus.plexus.util.FileUtils#getDefaultExcludes()} will also
   * be excluded.
   * 
   * @param root to scan
   * @param excludes exact matches to exclude from the returned list
   * @return a non-{@code null} list of directories matching the prefix
   */
  public static List<File> getDirectories(File root, Collection<String> excludes) {
    final HashSet<String> excludeStrings = new HashSet<String>(Arrays.asList(getDefaultExcludes()));
    excludeStrings.addAll(excludes);
    if (root != null && root.isDirectory()) {
      return new ArrayList<File>(Arrays.asList(root.listFiles(new FileFilter() {
        @Override
        public boolean accept(File file) {
          return file.isDirectory() && !excludeStrings.contains(file.getName());
        }
      })));
    }
    return new ArrayList<File>();
  }

  /**
   * Copies the contents of the source directory recursively into the
   * destination directory. The source directory must exist. All missing
   * directories including the destination folder will be created if necessary,
   * already existing files will be overwritten. By default patterns from
   * {@link org.codehaus.plexus.util.FileUtils#getDefaultExcludes()} will always
   * be excluded. Empty directories will be skipped.
   * 
   * @param from the source directory to copy from
   * @param to the destination directory to copy to
   * @param filter additional filter to apply before copying
   * @return a {@link Collection} of <b>files</b> that were copied
   * @throws MojoExecutionException
   * @see #getFilesAndDirectoriesRecursive(File, FileFilter)
   */
  public static Collection<File> copyDirectory(File from, File to, FileFilter filter) throws MojoExecutionException {
    Collection<File> copied = new ArrayList<File>();
    if (from.exists() && from.isDirectory()) {
      List<File> toCopy = getFilesAndDirectoriesRecursive(from, filter);
      for (File src : toCopy) {
        File dest = new File(to, src.getAbsolutePath().replace(from.getAbsolutePath(), ""));
        if (src.isDirectory()) {
          ensureDirectories(dest);
        }
        else {
          try {
            org.codehaus.plexus.util.FileUtils.copyFile(src, dest);
            copied.add(src);
          }
          catch (IOException e) {
            throw new MojoExecutionException(e.getMessage(), e);
          }
        }
      }
      for (File src : toCopy) {
        removeEmptyDirectory(new File(to, src.getAbsolutePath().replace(from.getAbsolutePath(), "")));
      }
    }
    return copied;
  }

  /**
   * Copies the contents of the source directory recursively into the
   * destination directory. The source directory must exist. All missing
   * directories including the destination folder will be created if necessary,
   * already existing files will be overwritten. The given replacements will be
   * applied to all found files. It is assumed that all files are {@code UTF-8}
   * encoded. By default patterns from
   * {@link org.codehaus.plexus.util.FileUtils#getDefaultExcludes()} will always
   * be excluded. Empty directories will be skipped.
   * 
   * @param from the source directory to copy from
   * @param to the destination directory to copy to
   * @param filter additional filter to apply before copying
   * @param replacements a {@link Map} of {@link String} patterns to be replaced
   * @return a {@link Collection} of <b>files</b> that were copied
   * @see #getFilesAndDirectoriesRecursive(File, FileFilter)
   * @throws MojoExecutionException
   */
  public static Collection<File> copyDirectory(File from,
                                               File to,
                                               FileFilter filter,
                                               Map<String, String> replacements) throws MojoExecutionException {
    Collection<File> copied = new ArrayList<File>();
    if (from.exists() && from.isDirectory()) {
      List<File> toCopy = getFilesAndDirectoriesRecursive(from, filter);
      for (File src : toCopy) {
        File dest = new File(to, src.getAbsolutePath().replace(from.getAbsolutePath(), ""));
        if (src.isDirectory()) {
          ensureDirectories(dest);
        }
        else {
          copyFile(src, dest, replacements);
          copied.add(src);
        }
      }
      for (File src : toCopy) {
        removeEmptyDirectory(new File(to, src.getAbsolutePath().replace(from.getAbsolutePath(), "")));
      }
    }
    return copied;
  }

  /**
   * Copies the content of a file into another file (previous content will be
   * overwritten). The source file must exist. All missing directories including
   * the destination folder will be created if necessary. The given replacements
   * will be applied to the content of the source file. It is assumed that the
   * file is {@code UTF-8} encoded.
   * 
   * @param from file to copy the content from
   * @param to file to copy the content into
   * @param replacements a {@link Map} of {@link String} patterns to be replaced
   * @throws IOException
   */
  public static void copyFile(File from, File to, Map<String, String> replacements) throws MojoExecutionException {
    try {
      String content = org.codehaus.plexus.util.FileUtils.fileRead(from, "UTF-8");
      for (Entry<String, String> replacement : replacements.entrySet()) {
        String value = replacement.getValue() == null ? "" : replacement.getValue();
        content = content.replace(replacement.getKey(), value);
      }
      File parent = to.getParentFile();
      if (parent != null) {
        ensureDirectories(parent);
      }
      writeFile(to, content);
    }
    catch (IOException e) {
      throw new MojoExecutionException(e.getMessage(), e);
    }
  }

  /**
   * Copies the given files to a specific destination directory. The destination
   * directory will be created if necessary.
   * 
   * @param destination directory to copy the files into
   * @param files to copy
   * @throws MojoExecutionException in case a file could not be copied
   */
  public static void copyFiles(File destination, File... files) throws MojoExecutionException {
    ensureDirectories(destination);
    for (File file : files) {
      File dest = new File(destination, file.getName());
      try {
        org.codehaus.plexus.util.FileUtils.copyFile(file, dest);
      }
      catch (IOException e) {
        throw new MojoExecutionException("Failed to copy " + file + ".");
      }
    }
  }

  /**
   * Writes data to a destination file. The file will be created if it doesn't
   * exist.
   * 
   * @param destination to write to
   * @param data to write
   * @throws MojoExecutionException
   */
  public static void writeFile(File destination, String data) throws MojoExecutionException {
    try {
      org.codehaus.plexus.util.FileUtils.fileWrite(destination.getAbsolutePath(), "UTF-8", data);
    }
    catch (IOException e) {
      throw new MojoExecutionException("Failed to write " + destination + ": " + e.getMessage());
    }
  }

  /**
   * Ensures that the directories denoted by the given files exist and are
   * directories. If a directory does not exist the function will try to create
   * it (with parent directories).
   * 
   * @param dirs to create/assure.
   * @throws MojoExecutionException in case one of the directories denotes a
   *           file or directory could not be created
   */
  public static void ensureDirectories(File... dirs) throws MojoExecutionException {
    for (File dir : dirs) {
      if (!dir.isDirectory()) {
        if (dir.isFile()) {
          throw new MojoExecutionException("Failed to create directory " + dir + " (is a file).");
        }
        else {
          if (!dir.mkdirs()) {
            throw new MojoExecutionException("Failed to create directory " + dir + ".");
          }
        }
      }
    }
  }

  /**
   * Touches a file UNIX style. If the file does not exist it will be created,
   * if the file already exists the modification timestamp is set to
   * {@link System#currentTimeMillis()}.
   * 
   * @param file to touch
   * @throws MojoExecutionException
   */
  public static void touch(File file) throws MojoExecutionException {
    if (file.isFile()) {
      if (!file.setLastModified(System.currentTimeMillis())) {
        throw new MojoExecutionException("Failed to update modification time on " + file + ".");
      }
    }
    else {
      try {
        if (!file.createNewFile()) {
          throw new MojoExecutionException("Failed to create " + file + ".");
        }
      }
      catch (IOException e) {
        throw new MojoExecutionException("Failed to create " + file + ".", e);
      }
    }
  }

  /**
   * Writes a file that is part of the classpath to a specific destination file.
   * Parent directories will be created if necessary.
   * 
   * @param clazz used to retrieve the classpath resource from
   * @param path the file's classpath prefix
   * @param name the file to extract denoted by its name in the classpath
   * @param dest the file to write to
   */
  public static void extractFileFromClassPath(Class<?> clazz, String path, String name, File dest) throws MojoExecutionException {
    if (dest.isDirectory()) {
      throw new MojoExecutionException("Destination must be a file.");
    }
    ensureDirectories(dest.getParentFile());
    writeFile(dest, readFileFromClassPath(clazz, path, name));
  }

  /**
   * Reads a file that is part of the classpath and returns its content as a
   * {@link String} object.
   * 
   * @param clazz used to retrieve the classpath resource from
   * @param path the file's classpath prefix
   * @param name the file to extract denoted by its name in the classpath
   * @return a non-{@code null} object containing the content of the file
   * @throws MojoExecutionException in case the file could not be found or read
   *           errors occured
   */
  public static String readFileFromClassPath(Class<?> clazz, String path, String name) throws MojoExecutionException {
    String resource = path + "/" + name;
    InputStream resourceStream = clazz.getResourceAsStream(resource);
    if (resourceStream != null) {
      StringBuilder data = new StringBuilder(4096);
      BufferedInputStream input = new BufferedInputStream(resourceStream);
      byte[] b = new byte[4096];
      try {
        while (true) {
          int numRead = input.read(b, 0, b.length);
          if (numRead == -1) {
            break;
          }
          data.append(new String(b, 0, numRead));
        }
      }
      catch (IOException e) {
        throw new MojoExecutionException("Failed to read from resource " + resource + ".", e);
      }
      finally {
        try {
          input.close();
        }
        catch (IOException e) {
          // ignored
        }
      }
      return data.toString();
    }
    else {
      throw new MojoExecutionException("Could not find resource " + resource + ".");
    }
  }
}
