package eu.lindenbaum.maven.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.junit.Test;

public class ErlUtilsTest {
  @Test
  public void testToListNull() {
    String result = ErlUtils.toList((String[]) null, null, "1", "2");
    assertEquals("[]", result);
  }

  @Test
  public void testToListNonNull() {
    Integer[] list = new Integer[]{ 1, 2, 3, 4, 5 };
    String result = ErlUtils.toList(list, new Predicate<Integer>() {
      @Override
      public boolean pred(Integer number) {
        return number % 2 == 0;
      }
    }, "'", "'");
    assertEquals("['2', '4']", result);
  }

  @Test
  public void testToFileListNull() {
    String result = ErlUtils.toFileList(new ArrayList<File>(), "1", "2");
    assertEquals("[]", result);
  }

  @Test
  public void testToFileListNonNull() {
    List<File> list = Arrays.asList(new File("pom.xml"), new File("non_existent"), new File("lgpl.txt"));
    String result = ErlUtils.toFileList(list, "1", "2");
    Pattern pattern = Pattern.compile("\\[1.+pom.xml2, 1.+lgpl.txt2\\]");
    Matcher matcher = pattern.matcher(result);
    assertTrue(matcher.matches());
  }

  @Test
  public void testToModuleListNull() {
    String result = ErlUtils.toModuleList(new ArrayList<File>(), "1", "2");
    assertEquals("[]", result);
  }
}
