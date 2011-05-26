package eu.lindenbaum.maven.util;

import org.junit.Test;

public class NetworkUtilsTest {
  @Test
  public void testRetrieveIP() throws Exception {
    String iPv4Address = NetworkUtils.getIPv4Address();
    System.out.println("Resolved address: " + iPv4Address);
  }
}
