### Prepare

Currently, a pinned version of `tcpip` is required to run the NAT device.  Get it as follows:

```
opam pin add tcpip https://github.com/yomimono/mirage-tcpip.git#expose_routing_exn
```

Additionally, there are some dependencies which are not available in the main OPAM repository.  They can be installed as follows:

```
opam pin add mirage-nat https://github.com/yomimono/mirage-nat.git
opam pin add irmin-network-datastores https://github.com/yomimono/irmin-network-datastores.git
opam pin add irmin-arp https://github.com/yomimono/irmin-arp.git
```

(optional)
```
opam pin add lwt 'https://github.com/mirage/lwt.git#tracing
```

### Compile

```
mirage configure --xen # for a virtual machine
make
```
