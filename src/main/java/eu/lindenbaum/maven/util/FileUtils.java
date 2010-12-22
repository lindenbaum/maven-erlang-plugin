package eu.lindenbaum.maven.util;

import static eu.lindenbaum.maven.util.ErlConstants.APPUP_SUFFIX;
import static eu.lindenbaum.maven.util.ErlConstants.APP_SUFFIX;
import static eu.lindenbaum.maven.util.ErlConstants.BEAM_SUFFIX;
import static eu.lindenbaum.maven.util.ErlConstants.ERL_SUFFIX;
import static eu.lindenbaum.maven.util.ErlConstants.HRL_SUFFIX;
import static eu.lindenbaum.maven.util.ErlConstants.REL_SUFFIX;
import static org.codehaus.plexus.util.FileUtils.copyFile;
import static org.codehaus.plexus.util.FileUtils.deleteDirectory;
import static org.codehaus.plexus.util.FileUtils.fileRead;
import static org.codehaus.plexus.util.FileUtils.fileWrite;
import static org.codehaus.plexus.util.FileUtils.getDefaultExcludes;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileFilter;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;

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
    int removed = 0;
    List<File> files = getFilesRecursive(root, suffix);
    for (File file : files) {
      if (file.exists() && file.delete()) {
        removed++;
      }
    }
    return removed;
  }

  /**
   * Removes a directory recursively.
   * 
   * @param directory to remove
   */
  public static void removeDirectory(File directory) {
    try {
      deleteDirectory(directory);
    }
    catch (IOException e) {
      // ignore
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
   * @return the number of copied <b>files</b>
   * @throws MojoExecutionException
   * @see #getFilesAndDirectoriesRecursive(File, FileFilter)
   */
  public static int copyDirectory(File from, File to, FileFilter filter) throws MojoExecutionException {
    int copied = 0;
    if (from.exists() && from.isDirectory()) {
      List<File> toCopy = getFilesAndDirectoriesRecursive(from, filter);
      for (File src : toCopy) {
        File dest = new File(to, src.getAbsolutePath().replace(from.getAbsolutePath(), ""));
        if (src.isDirectory()) {
          dest.mkdirs();
        }
        else {
          try {
            copyFile(src, dest);
            copied++;
          }
          catch (IOException e) {
            throw new MojoExecutionException(e.getMessage(), e);
          }
        }
      }
      for (File src : toCopy) {
        File dest = new File(to, src.getAbsolutePath().replace(from.getAbsolutePath(), ""));
        if (dest.isDirectory()) {
          dest.delete();
        }
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
   * @return the number of copied <b>files</b>
   * @see #getFilesAndDirectoriesRecursive(File, FileFilter)
   * @throws MojoExecutionException
   */
  public static int copyDirectory(File from, File to, FileFilter filter, Map<String, String> replacements) throws MojoExecutionException {
    int copied = 0;
    if (from.exists() && from.isDirectory()) {
      List<File> toCopy = getFilesAndDirectoriesRecursive(from, filter);
      for (File src : toCopy) {
        File dest = new File(to, src.getAbsolutePath().replace(from.getAbsolutePath(), ""));
        if (src.isDirectory()) {
          dest.mkdirs();
        }
        else {
          try {
            String content = fileRead(src, "UTF-8");
            for (Entry<String, String> replacement : replacements.entrySet()) {
              String value = replacement.getValue() == null ? "" : replacement.getValue();
              content = content.replace(replacement.getKey(), value);
            }
            File parent = dest.getParentFile();
            if (parent != null) {
              parent.mkdirs();
            }
            fileWrite(dest.getAbsolutePath(), "UTF-8", content);
            copied++;
          }
          catch (IOException e) {
            throw new MojoExecutionException(e.getMessage(), e);
          }
        }
      }
      for (File src : toCopy) {
        File dest = new File(to, src.getAbsolutePath().replace(from.getAbsolutePath(), ""));
        if (dest.isDirectory()) {
          dest.delete();
        }
      }
    }
    return copied;
  }

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
      List<File> sources = getFilesRecursive(dir, ERL_SUFFIX);
      sources.addAll(getFilesRecursive(dir, HRL_SUFFIX));
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
   * Extracts all files from a .jar file matching the given file suffix into a
   * directory.
   * 
   * @param archive the jar archive to extract files from
   * @param suffix of files to be extracted
   * @param destDir to extract files into
   * @throws MojoExecutionException
   */
  public static void extractFilesFromJar(File archive, String suffix, File destDir) throws MojoExecutionException {
    destDir.mkdirs();
    if (destDir.isDirectory()) {
      try {
        JarFile jarFile = new JarFile(archive);
        Enumeration<JarEntry> entries = jarFile.entries();
        List<JarEntry> toBeExtracted = new ArrayList<JarEntry>();
        while (entries.hasMoreElements()) {
          JarEntry current = entries.nextElement();
          if (current.getName().endsWith(suffix)) {
            toBeExtracted.add(current);
          }
        }
        for (JarEntry entry : toBeExtracted) {
          File dest = new File(destDir, new File(entry.getName()).getName());
          OutputStream outputStream = new BufferedOutputStream(new FileOutputStream(dest));
          InputStream inputStream = new BufferedInputStream(jarFile.getInputStream(entry));
          byte[] buffer = new byte[4096];
          while (true) {
            int nBytes = inputStream.read(buffer);
            if (nBytes <= 0) {
              break;
            }
            outputStream.write(buffer, 0, nBytes);
          }
          outputStream.flush();
          outputStream.close();
          inputStream.close();
        }
      }
      catch (IOException e) {
        throw new MojoExecutionException(e.getMessage(), e);
      }
    }
    else {
      throw new MojoExecutionException("directory " + destDir + " cannot be created");
    }
  }
}
