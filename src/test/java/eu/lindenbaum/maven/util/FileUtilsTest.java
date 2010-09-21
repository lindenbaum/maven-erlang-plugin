package eu.lindenbaum.maven.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.FileFilter;
import java.net.URL;
import java.util.List;

import org.codehaus.plexus.util.SelectorUtils;
import org.junit.Test;

public class FileUtilsTest {
  @Test
  public void testGetFilesRecursive() throws Exception {
    URL resource = getClass().getClassLoader().getResource("file-utils");
    File root = new File(resource.getFile());
    List<File> files = FileUtils.getFilesRecursive(root, ".txt");
    assertEquals(2, files.size());
    assertEquals("file1.txt", files.get(0).getName());
    assertEquals("file0.txt", files.get(1).getName());
  }

  @Test
  public void testGetDirectoriesRecursive() throws Exception {
    URL resource = getClass().getClassLoader().getResource("file-utils");
    File root = new File(resource.getFile());
    List<File> files = FileUtils.getDirectoriesRecursive(root, new FileFilter() {
      @Override
      public boolean accept(File pathname) {
        return "ebin".equals(pathname.getName());
      }
    });
    assertEquals(3, files.size());
  }

  @Test
  public void testGetFilesAndDirectoriesRecursive() throws Exception {
    URL resource = getClass().getClassLoader().getResource("file-utils");
    File root = new File(resource.getFile());
    final String[] excludes = org.codehaus.plexus.util.FileUtils.getDefaultExcludes();
    List<File> files = FileUtils.getFilesAndDirectoriesRecursive(root, new FileFilter() {
      @Override
      public boolean accept(File pathname) {
        for (String exclude : excludes) {
          if (SelectorUtils.match(exclude, pathname.getAbsolutePath())) {
            return false;
          }
        }
        return true;
      }
    });
    assertEquals(12, files.size());
  }

  @Test
  public void testOtpDirectoryRegex() throws Exception {
    assertTrue(FileUtils.APP_PATTERN.matcher("test-path/app-1.0/ebin").matches());
    assertTrue(FileUtils.APP_PATTERN.matcher("path/app-1.0/ebin").matches());
    assertFalse(FileUtils.APP_PATTERN.matcher("path/app/ebin").matches());
    assertFalse(FileUtils.APP_PATTERN.matcher("test-path/app/ebin").matches());
  }

  @Test
  public void testGetDependencies() throws Exception {
    URL resource = getClass().getClassLoader().getResource("file-utils");
    File root = new File(resource.getFile());
    List<File> files = FileUtils.getDependencies(root);
    assertEquals(1, files.size());
  }
}
