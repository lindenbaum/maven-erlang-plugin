package eu.lindenbaum.maven.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangInt;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangUInt;

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
    String result = ErlUtils.toFilenameList(new ArrayList<File>(), "1", "2");
    assertEquals("[]", result);
  }

  @Test
  public void testToFileListNonNull() {
    List<File> list = Arrays.asList(new File("pom.xml"), new File("non_existent"), new File("lgpl.txt"));
    String result = ErlUtils.toFilenameList(list, "1", "2");
    Pattern pattern = Pattern.compile("\\[1.+pom.xml2, 1.+lgpl.txt2\\]");
    Matcher matcher = pattern.matcher(result);
    assertTrue(matcher.matches());
  }

  @Test
  public void testToModuleListNull() {
    String result = ErlUtils.toModuleList(new ArrayList<File>(), "1", "2");
    assertEquals("[]", result);
  }

  @Test(expected = NullPointerException.class)
  public void testToStringIsNotNullSafe() {
    ErlUtils.toString(null);
  }

  @Test
  public void testStringToString() {
    assertEquals("foobar", ErlUtils.toString(new OtpErlangString("foobar")));
  }

  @Test
  public void testAtomToString() {
    assertEquals("foobar", ErlUtils.toString(new OtpErlangAtom("foobar")));
  }

  @Test
  public void testListToString() {
    OtpErlangInt one = new OtpErlangInt(1);
    OtpErlangInt two = new OtpErlangInt(2);
    OtpErlangInt three = new OtpErlangInt(3);
    assertEquals("[1,2,3]", ErlUtils.toString(new OtpErlangList(new OtpErlangObject[]{ one, two, three })));
  }

  @Test
  public void testEmptyListToString() {
    assertEquals("", ErlUtils.toString(new OtpErlangList()));
  }

  @Test
  public void ensureToIntIsNullSafe() {
    assertEquals(0, ErlUtils.toInt(null));
  }

  @Test
  public void ensureToIntDiscardsBadType() {
    assertEquals(0, ErlUtils.toInt(new OtpErlangString("123")));
    assertEquals(0, ErlUtils.toInt(new OtpErlangAtom("123")));
  }

  @Test
  public void ensureToIntSilentlyEatsOverflowException() {
    assertEquals(0, ErlUtils.toInt(new OtpErlangLong(Long.MAX_VALUE)));
    assertEquals(0, ErlUtils.toInt(new OtpErlangLong(Long.MIN_VALUE)));
  }

  @Test
  public void testToInt() throws OtpErlangRangeException {
    assertEquals(0, ErlUtils.toInt(new OtpErlangLong(0)));
    assertEquals(-123, ErlUtils.toInt(new OtpErlangLong(-123)));
    assertEquals(123, ErlUtils.toInt(new OtpErlangLong(123)));
    assertEquals(Integer.MIN_VALUE, ErlUtils.toInt(new OtpErlangLong(Integer.MIN_VALUE)));
    assertEquals(Integer.MAX_VALUE, ErlUtils.toInt(new OtpErlangLong(Integer.MAX_VALUE)));

    assertEquals(0, ErlUtils.toInt(new OtpErlangInt(0)));
    assertEquals(-123, ErlUtils.toInt(new OtpErlangInt(-123)));
    assertEquals(123, ErlUtils.toInt(new OtpErlangInt(123)));
    assertEquals(Integer.MAX_VALUE, ErlUtils.toInt(new OtpErlangInt(Integer.MAX_VALUE)));
    assertEquals(Integer.MIN_VALUE, ErlUtils.toInt(new OtpErlangInt(Integer.MIN_VALUE)));

    assertEquals(0, ErlUtils.toInt(new OtpErlangUInt(0)));
    assertEquals(123, ErlUtils.toInt(new OtpErlangUInt(123)));
    assertEquals(Integer.MAX_VALUE, ErlUtils.toInt(new OtpErlangUInt(Integer.MAX_VALUE)));
  }
}
