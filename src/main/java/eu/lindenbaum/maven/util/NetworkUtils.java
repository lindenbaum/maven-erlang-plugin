package eu.lindenbaum.maven.util;

import java.net.Inet4Address;
import java.net.Inet6Address;
import java.net.InetAddress;
import java.net.NetworkInterface;
import java.net.SocketException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Enumeration;
import java.util.Iterator;
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
    List<InetAddress> inetAddresses = getInetAddresses();
    filterInetAddresses(inetAddresses, true);
    sortInetAddresses(inetAddresses);
    if (inetAddresses.isEmpty()) {
      throw new MojoExecutionException("Couldn't resolve any IPv4 addresses.");
    }
    return inetAddresses.get(0).getHostAddress();
  }

  /**
   * Returns the {@link InetAddress}es as returned by the network interfaces of
   * this host. Addresses of network interfaces currently not up are excluded.
   * 
   * @return a non-{@code null} list of network addresses.
   * @throws MojoExecutionException
   */
  public static List<InetAddress> getInetAddresses() throws MojoExecutionException {
    ArrayList<InetAddress> addressList = new ArrayList<InetAddress>();
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
   * Filters the given {@link List} of {@link InetAddress}es for IPv4 or IPv6
   * addresses. The input list will be modified. Multicast and wildcard
   * addresses will be excluded.
   * 
   * @param addresses to filter for IPv4/IPv6 addresses
   * @param ipv4only if {@code true} the {@link List} will only contain IPv4
   *          addresses, otherwise only IPv6 addresses will be contained
   * @return the modified input {@link List}
   */
  public static List<InetAddress> filterInetAddresses(List<InetAddress> addresses, boolean ipv4only) {
    Iterator<InetAddress> iterator = addresses.iterator();
    while (iterator.hasNext()) {
      InetAddress address = iterator.next();
      if (address.isAnyLocalAddress() //
          || address.isMulticastAddress() //
          || ipv4only && !(address instanceof Inet4Address) //
          || !ipv4only && !(address instanceof Inet6Address)) {
        iterator.remove();
      }
    }
    return addresses;
  }

  /**
   * Orders the given {@link List} of {@link InetAddress}es in the way that all
   * loopback addresses are put to the end of the list. The input {@link List}
   * will be modified.
   * 
   * @param addresses {@link List} of addresses to reorder
   * @return the modified input {@link List}
   */
  public static List<InetAddress> sortInetAddresses(List<InetAddress> addresses) {
    Collections.sort(addresses, new Comparator<InetAddress>() {
      @Override
      public int compare(InetAddress o1, InetAddress o2) {
        return o1.isLoopbackAddress() ? (o2.isLoopbackAddress() ? 0 : 1) : -1;
      }
    });
    return addresses;
  }
}
