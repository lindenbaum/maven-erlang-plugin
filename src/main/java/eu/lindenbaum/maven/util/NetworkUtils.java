package eu.lindenbaum.maven.util;

import java.net.Inet4Address;
import java.net.Inet6Address;
import java.net.InetAddress;
import java.net.NetworkInterface;
import java.net.SocketException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Enumeration;
import java.util.List;

import org.apache.maven.plugin.MojoExecutionException;

/**
 * Containing network related utilities for IP resolution.
 * 
 * @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
 * @author Timo Koepke <timo.koepke@lindenbaum.eu>
 * @since 2.1.0
 */
public final class NetworkUtils {
  /**
   * Returns a valid IPv4 address for this host. Only addresses of currently up
   * network interfaces are considered. The returned address might be a loopback
   * address. A loopback address will only be returned if no other address could
   * be resolved.
   * 
   * @return a non-{@code null} string containing one network address of this
   *         host
   * @throws MojoExecutionException in case no up network interfaces could be
   *           detected
   */
  public static String getIPv4Address() throws MojoExecutionException {
    Collection<InetAddress> addresses = filterInetAddresses(getInetAddresses(), true);
    Collection<InetAddress> sorted = sortInetAddresses(addresses);
    for (InetAddress address : sorted) {
      return address.getHostAddress();
    }
    throw new MojoExecutionException("Couldn't resolve any IPv4 addresses.");
  }

  /**
   * Returns the {@link InetAddress}es as returned by the network interfaces of
   * this host. Addresses of network interfaces currently not up are excluded.
   * 
   * @return a non-{@code null} collection of network addresses.
   * @throws MojoExecutionException
   */
  public static Collection<InetAddress> getInetAddresses() throws MojoExecutionException {
    Collection<InetAddress> addressList = new ArrayList<InetAddress>();
    try {
      Enumeration<NetworkInterface> interfaces = NetworkInterface.getNetworkInterfaces();
      while (interfaces.hasMoreElements()) {
        NetworkInterface i = interfaces.nextElement();
        if (i.isUp()) {
          Enumeration<InetAddress> addresses = i.getInetAddresses();
          while (addresses.hasMoreElements()) {
            addressList.add(addresses.nextElement());
          }
        }
      }
      return addressList;
    }
    catch (SocketException e) {
      throw new MojoExecutionException("Failed to retrieve network interfaces.", e);
    }
  }

  /**
   * Filters the elements of the given {@link Collection} of {@link InetAddress}
   * objects for IPv4 or IPv6 addresses. The input collection will not be
   * modified, instead a new {@link Collection} will be returned. Multicast and
   * wildcard addresses will be filtered out.
   * 
   * @param in to filter for IPv4/IPv6 addresses
   * @param ipv4only if {@code true} the {@link List} will only contain IPv4
   *          addresses, otherwise only IPv6 addresses will be contained
   * @return a filtered {@link Collection}
   */
  public static Collection<InetAddress> filterInetAddresses(Collection<InetAddress> in, boolean ipv4only) {
    ArrayList<InetAddress> filtered = new ArrayList<InetAddress>();
    for (InetAddress address : in) {
      if (!address.isAnyLocalAddress() && !address.isMulticastAddress()) {
        if (ipv4only && address instanceof Inet4Address //
            || !ipv4only && address instanceof Inet6Address) {
          filtered.add(address);
        }
      }
    }
    return filtered;
  }

  /**
   * <p>
   * Orders the elements of the given {@link Collection}. The input
   * {@link Collection} will not be modified, instead a new {@link Collection}
   * is returned. Output order is as followed:
   * <ol>
   * <li>public network IP addresses</li>
   * <li>private network IP addresses (192.x.x.x preferred)</li>
   * <li>loopback addresses (e.g. 127.0.0.1)</li>
   * </ol>
   * </p>
   * 
   * @param in {@link Collection} of addresses to reorder
   * @return a sorted {@link Collection}
   */
  public static Collection<InetAddress> sortInetAddresses(Collection<InetAddress> in) {
    List<InetAddress> sorted = new ArrayList<InetAddress>(in);
    Collections.sort(sorted, new Comparator<InetAddress>() {
      @Override
      public int compare(InetAddress o1, InetAddress o2) {
        boolean l1 = o1.isLoopbackAddress();
        boolean l2 = o2.isLoopbackAddress();
        boolean s1 = o1.isSiteLocalAddress();
        boolean s2 = o2.isSiteLocalAddress();
        boolean i1 = o1.getHostAddress().startsWith("192");
        boolean i2 = o2.getHostAddress().startsWith("192");

        int is192 = i1 ? (i2 ? 0 : -1) : i2 ? 1 : 0;
        int isSiteLocal = s1 ? (s2 ? is192 : 1) : s2 ? -1 : 0;
        return l1 ? (l2 ? 0 : 1) : l2 ? -1 : isSiteLocal;
      }
    });
    return sorted;
  }
}
