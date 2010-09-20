package eu.lindenbaum.maven.util;

import static org.junit.Assert.assertEquals;

import java.io.File;
import java.net.URL;
import java.util.List;

import org.junit.Test;

public class FileUtilsTest {
  @Test
  public void testname() throws Exception {
    URL resource = getClass().getClassLoader().getResource("file-utils");
    File root = new File(resource.getFile());
    List<File> files = FileUtils.getFilesRecursive(root, ".txt");
    assertEquals(2, files.size());
    assertEquals("file1.txt", files.get(0).getName());
    assertEquals("file0.txt", files.get(1).getName());
  }
}
