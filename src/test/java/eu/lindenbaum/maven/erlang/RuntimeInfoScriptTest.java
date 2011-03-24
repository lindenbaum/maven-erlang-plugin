package eu.lindenbaum.maven.erlang;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.util.Arrays;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

import org.apache.maven.plugin.MojoExecutionException;
import org.junit.Test;

public class RuntimeInfoScriptTest {
  @Test
  public void testGet() throws MojoExecutionException {
    RuntimeInfoScript script = new RuntimeInfoScript();
    String expression = script.get();
    assertNotNull(expression);
    assertFalse(expression.isEmpty());
    assertFalse(expression.contains("%s"));
  }

  @Test
  public void testHandle() throws MojoExecutionException {
    OtpErlangString path1 = new OtpErlangString("/path1");
    OtpErlangString path2 = new OtpErlangString("/path2");
    OtpErlangString version = new OtpErlangString("version");
    OtpErlangString release = new OtpErlangString("release");
    OtpErlangList paths = new OtpErlangList(new OtpErlangObject[]{ path1, path2 });
    OtpErlangTuple result = new OtpErlangTuple(new OtpErlangObject[]{ path1, path2, version, release, paths });

    RuntimeInfoScript script = new RuntimeInfoScript();
    RuntimeInfo info = script.handle(result);
    assertNotNull(info);
    assertEquals("path1", info.getLibDirectory().getName());
    assertEquals("path2", info.getRootDirectory().getName());
    assertEquals("version", info.getVersion());
    assertEquals("release", info.getOtpRelease());
    assertTrue(info.getPaths().containsAll(Arrays.asList(new File("/path1"), new File("/path2"))));
  }
}
