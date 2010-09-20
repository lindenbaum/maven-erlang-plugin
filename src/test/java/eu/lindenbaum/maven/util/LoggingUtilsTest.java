package eu.lindenbaum.maven.util;

import static org.easymock.EasyMock.createStrictControl;
import static org.easymock.EasyMock.expect;

import org.apache.maven.plugin.logging.Log;
import org.easymock.IMocksControl;
import org.junit.Before;
import org.junit.Test;

public class LoggingUtilsTest {
  private IMocksControl control;
  private Log log;

  @Before
  public void setUp() throws Exception {
    this.control = createStrictControl();
    this.log = this.control.createMock("log", Log.class);
  }

  @Test
  public void testLogDebug() throws Exception {
    expect(this.log.isDebugEnabled()).andReturn(true);
    this.log.debug("visible");
    expect(this.log.isDebugEnabled()).andReturn(false);

    this.control.replay();

    LoggingUtils.logDebug(this.log, "visible");
    LoggingUtils.logDebug(this.log, "invisible");

    this.control.verify();
  }

}
