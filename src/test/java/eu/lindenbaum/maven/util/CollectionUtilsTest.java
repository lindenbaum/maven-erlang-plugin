package eu.lindenbaum.maven.util;

import static org.junit.Assert.assertEquals;

import java.util.Arrays;
import java.util.Collection;

import eu.lindenbaum.maven.util.CollectionUtils.FoldFunction;
import eu.lindenbaum.maven.util.CollectionUtils.MapFunction;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;

import org.junit.Test;

public class CollectionUtilsTest {
  @Test
  public void testMapMapFunctionOfABCollectionOfA() {
    Collection<String> input = Arrays.asList("1", "2", "3");
    Collection<Integer> result = CollectionUtils.map(new MapFunction<String, Integer>() {
      @Override
      public Integer apply(String a) {
        return Integer.valueOf(a);
      }
    }, input);
    assertEquals(Arrays.asList(1, 2, 3), result);
  }

  @Test
  public void testMapMapFunctionOfOtpErlangObjectBOtpErlangList() {
    OtpErlangAtom a = new OtpErlangAtom("a");
    OtpErlangAtom b = new OtpErlangAtom("b");
    OtpErlangAtom c = new OtpErlangAtom("c");
    OtpErlangList input = new OtpErlangList(new OtpErlangObject[]{ a, b, c });
    Collection<String> result = CollectionUtils.map(new MapFunction<OtpErlangObject, String>() {
      @Override
      public String apply(OtpErlangObject a) {
        return a.toString();
      }
    }, input);
    assertEquals(Arrays.asList("a", "b", "c"), result);
  }

  @Test
  public void testFilter() {
    Collection<String> input = Arrays.asList("1", "2", "3");
    Collection<String> result = CollectionUtils.filter(new Predicate<String>() {
      @Override
      public boolean pred(String object) {
        return "1".equals(object) || "3".equals(object);
      }
    }, input);
    assertEquals(Arrays.asList("1", "3"), result);
  }

  @Test
  public void testFoldl() {
    Collection<String> input = Arrays.asList("1", "2", "3");
    String result = CollectionUtils.foldl(new FoldFunction<String, String>() {
      @Override
      public String apply(String a, String acc) {
        return acc + a;
      }
    }, "", input);
    assertEquals("123", result);
  }
}
