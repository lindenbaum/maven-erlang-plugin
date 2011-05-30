package eu.lindenbaum.maven.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.net.InetAddress;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import org.junit.Test;

public class NetworkUtilsTest {
  @Test
  public void testRetrieveIP() throws Exception {
    String iPv4Address = NetworkUtils.getIPv4Address();
    System.out.println("Resolved address: " + iPv4Address);
  }

  @Test
  public void testSortInetAddresses() throws Exception {
    InetAddress lo = InetAddress.getByName("127.0.0.1");
    assertTrue(lo.isLoopbackAddress());
    assertFalse(lo.isSiteLocalAddress());
    InetAddress eth0 = InetAddress.getByName("192.168.0.1");
    assertFalse(eth0.isLoopbackAddress());
    assertTrue(eth0.isSiteLocalAddress());
    InetAddress eth1 = InetAddress.getByName("10.0.2.1");
    assertFalse(eth1.isLoopbackAddress());
    assertTrue(eth1.isSiteLocalAddress());
    InetAddress eth2 = InetAddress.getByName("169.254.158.68");
    assertFalse(eth2.isLoopbackAddress());
    assertFalse(eth2.isSiteLocalAddress());
    InetAddress eth3 = InetAddress.getByName("213.5.87.1");
    assertFalse(eth3.isLoopbackAddress());
    assertFalse(eth3.isSiteLocalAddress());

    List<InetAddress> inetAddresses = new ArrayList<InetAddress>(Arrays.asList(eth2, eth3, eth1, eth0, lo));
    Collection<InetAddress> sorted = NetworkUtils.sortInetAddresses(inetAddresses);
    List<InetAddress> expected = Arrays.asList(eth2, eth3, eth0, eth1, lo);
    assertEquals(expected, sorted);
  }
}
