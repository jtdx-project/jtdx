#ifndef NETWORK_SERVER_LOOKUP_HPP__
#define NETWORK_SERVER_LOOKUP_HPP__

#include <tuple>

#include <QHostAddress>
#include <QAbstractSocket>

class QString;

//
// Do a blocking DNS lookup using query as a destination host address
// and port.
//
// query can be one of:
//
// 1) "" (empty string) - use defaults
// 2) ":nnnnn" - override default service port with port nnnnn
// 3) "<valid-host-name>" - override default host address with DNS lookup
// 4) "nnn.nnn.nnn.nnn" - override default host address with the IPv4 address given by nnn.nnn.nnn.nnn
// 5) "[<valid-IPv6-address]" - override default host address with the given IPv6 address
// 6) "<valid-host-name>:nnnnn" - use as per (3) & (2)
// 7) "nnn.nnn.nnn.nnn:nnnnn" - use as per (4) & (2)
// 8) "[<valid-IPv6-address]:nnnnn" - use as per (5) & (2)
//
// The first host address matching the protocol and the service port
// number are returned.
//
// If no suitable host address is found QHostAddress::Null will be
// returned in the first member of the result tuple.
//
std::tuple<QHostAddress, quint16>
network_server_lookup (QString query
		       , quint16 default_service_port
		       , QHostAddress default_host_address = QHostAddress::LocalHost
		       , QAbstractSocket::NetworkLayerProtocol protocol = QAbstractSocket::AnyIPProtocol);

#endif
