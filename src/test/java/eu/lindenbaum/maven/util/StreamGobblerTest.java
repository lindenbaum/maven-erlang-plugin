package eu.lindenbaum.maven.util;

import static org.easymock.EasyMock.createStrictControl;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.io.InputStream;
import java.util.List;

import org.apache.maven.plugin.logging.Log;
import org.easymock.IMocksControl;
import org.junit.Before;
import org.junit.Test;

public class StreamGobblerTest {
  private IMocksControl control;
  private Log log;

  @Before
  public void setUp() throws Exception {
    this.control = createStrictControl();
    this.log = this.control.createMock("log", Log.class);
  }

  @Test
  public void testNonEmpty() throws Exception {
    InputStream inputStream = getClass().getClassLoader().getResourceAsStream("stream-gobbler-test.txt");

    this.log.info("line1");
    this.log.info("line2");

    this.control.replay();

    StreamGobbler gobbler = new StreamGobbler(inputStream, this.log);
    assertTrue(gobbler.getLines().isEmpty());
    gobbler.run();
    List<String> lines = gobbler.getLines();
    assertEquals(2, lines.size());
    assertEquals("line1", lines.get(0));
    assertEquals("line2", lines.get(1));

    this.control.verify();
  }
}
