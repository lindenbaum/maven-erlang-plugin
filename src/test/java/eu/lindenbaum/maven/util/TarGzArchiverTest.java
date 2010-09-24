package eu.lindenbaum.maven.util;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;
import java.io.IOException;
import java.net.URL;

import org.apache.maven.plugin.logging.SystemStreamLog;
import org.junit.Test;

public class TarGzArchiverTest {
  @Test
  public void testNoFiles() {
    SystemStreamLog log = new SystemStreamLog();
    URL resource = getClass().getClassLoader().getResource("tar-gz-unarchiver");
    File testRoot = new File(resource.getFile());
    File archive = new File(testRoot, "archive.tar.gz");
    archive.delete();

    TarGzArchiver archiver = new TarGzArchiver(log, archive);
    try {
      archiver.createArchive();
      fail("IOException expected");
    }
    catch (IOException expected) {
    }

    assertFalse(archive.exists());
  }

  @Test
  public void testSuccess() throws Exception {
    SystemStreamLog log = new SystemStreamLog();
    URL resource = getClass().getClassLoader().getResource("tar-gz-unarchiver");
    File testRoot = new File(resource.getFile());
    File archive = new File(testRoot, "archive.tar.gz");
    assertFalse(archive.exists());
    archive.deleteOnExit();

    TarGzArchiver archiver = new TarGzArchiver(log, archive);
    File toBeArchived = new File(resource.getFile());
    archiver.addFile(toBeArchived, "tar-gz-archived");
    archiver.createArchive();

    assertTrue(archive.isFile());
  }
}
