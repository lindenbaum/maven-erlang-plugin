package eu.lindenbaum.maven.util;

import static org.easymock.EasyMock.anyObject;
import static org.easymock.EasyMock.createStrictControl;
import static org.easymock.EasyMock.expect;
import static org.easymock.EasyMock.expectLastCall;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;

import java.util.List;

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;
import org.easymock.IMocksControl;
import org.junit.Before;
import org.junit.Test;

public class ErlUtilsTest {
  private IMocksControl control;
  private Log log;

  @Before
  public void setUp() throws Exception {
    this.control = createStrictControl();
    this.log = this.control.createMock("log", Log.class);

    expect(this.log.isDebugEnabled()).andStubReturn(true);
    this.log.debug((CharSequence) anyObject());
    expectLastCall().asStub();
  }

  @Test
  public void testExec() throws Exception {
    this.control.replay();

    assertEquals("ok", ErlUtils.exec(new String[]{ "dir" }, this.log, null, new ProcessListener() {
      @Override
      public String processCompleted(int exitValue, List<String> processOutput) throws MojoExecutionException {
        assertEquals(exitValue, 0);
        assertFalse(processOutput.isEmpty());
        return "ok";
      }
    }));

    this.control.verify();
  }

  @Test
  public void testEval2() throws Exception {
    this.control.replay();

    assertEquals("ok", ErlUtils.eval(this.log, "io:format(\"ok~n\")"));

    this.control.verify();
  }

  @Test
  public void testEval3() throws Exception {
    this.control.replay();

    assertEquals("ok", ErlUtils.eval(this.log, "io:format(\"ok~n\")", null));

    this.control.verify();
  }

  @Test
  public void testEval4() throws Exception {
    this.control.replay();

    assertEquals("ok", ErlUtils.eval(this.log, "io:format(\"ok~n\")", null, null));

    this.control.verify();
  }
}
