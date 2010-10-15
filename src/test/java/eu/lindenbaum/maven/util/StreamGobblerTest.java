package eu.lindenbaum.maven.util;

import static org.easymock.EasyMock.createStrictControl;

import java.io.File;
import java.io.InputStream;

import org.easymock.IMocksControl;
import org.junit.Before;
import org.junit.Test;

public class StreamGobblerTest {
  private IMocksControl control;
  private Processor processor;

  @Before
  public void setUp() {
    this.control = createStrictControl();
    this.processor = this.control.createMock("outputProcessor", Processor.class);
  }

  @Test
  public void testEmpty() {
    String file = "stream-gobbler" + File.separator + "empty.txt";
    InputStream inputStream = getClass().getClassLoader().getResourceAsStream(file);

    this.control.replay();

    StreamGobbler gobbler = new StreamGobbler(inputStream, this.processor);
    gobbler.run();

    this.control.verify();
  }

  @Test
  public void testNonEmpty() {
    String file = "stream-gobbler" + File.separator + "non-empty.txt";
    InputStream inputStream = getClass().getClassLoader().getResourceAsStream(file);

    this.processor.handle("line1");
    this.processor.handle("line2");

    this.control.replay();

    StreamGobbler gobbler = new StreamGobbler(inputStream, this.processor);
    gobbler.run();

    this.control.verify();
  }
}
