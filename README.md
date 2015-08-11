### Prepare

Currently, a pinned version of `tcpip` is required to run the NAT device.  Get it as follows:

```
opam pin add tcpip https://github.com/yomimono/mirage-tcpip.git#expose_routing_exn
```

Pinned versions of `dolog` and `bin_prot` are also necessary to run applications using Irmin as unikernels.  Get them here:

```
opam pin add dolog https://github.com/unixjunkie/dolog.git#no_unix
opam pin add bin_prot https://github.com/samoht/bin_prot.git#112.35.00+xen
```

Additionally, there are some dependencies which are not available in the main OPAM repository.  They can be installed as follows:

```
opam pin add irmin-network-datastores https://github.com/yomimono/irmin-network-datastores.git
opam pin add mirage-nat https://github.com/yomimono/mirage-nat.git
opam pin add irmin-arp https://github.com/yomimono/irmin-arp.git
```

(optional)
```
opam pin add lwt 'https://github.com/mirage/lwt.git#tracing
```

You'll need `mirage` itself as well:

```
opam install mirage
```

### Compile

```
mirage configure --xen # for a virtual machine
make
```

### Run

For an example configuration and invocation of the NAT device, see `multibridge.xl` and `multibridge.sh`.
