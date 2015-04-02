# To NAT Traffic Through This Unikernel on a CubieBoard2 or CubieTruck

These instructions assume you are using an external USB WiFi dongle with a CubieBoard2 or CubieTruck running Xen on a Linux Dom0, although it's also possible to use the same strategy with an x86 Xen machine or a device with two Ethernet interfaces.

## Preconditions
* follow the instructions at [xen-arm-builder](http://github.com/mirage/xen-arm-builder) to get a Xen hypervisor and OCaml toolchain set up on your CubieThing, then ssh into it with the default credentials
* make a personal account and set its password with `passwd`, then add it to sudoers with `visudo`
* get the Cubie access to the internet via Ethernet by your favorite method (if your favorite method doesn't involve the Ethernet interface, be sure to add the interface which does have access to bridge `br0`)

## Dom0 Setup

* install `hostapd` (for wireless access point) and `dnsmasq` (for DHCP server)
```
sudo apt-get install hostapd dnsmasq
```
* `apt` will start `dnsmasq` right after it's installed.  You don't want it to be running yet as it's not properly configured, so kill it:

```
sudo kill `pidof dnsmasq`
```

* create the file `/etc/hostapd.conf` and add these lines to it (assuming you want to offer an open WiFi access point named "free wifi for humans not a trap" on channel 11; if you want something else, change these values):

```
interface=wlan0
ssid=free wifi for humans not a trap
channel=11
```

* initialize the wireless USB dongle:

```
sudo ip link set wlan0 up
```

* start hostapd:

```
sudo hostapd -B /etc/hostapd.conf
```

Output should look something like this (the important part being "AP-ENABLED"):

```
Configuration file: /etc/hostapd.conf
Using interface wlan0 with hwaddr 00:04:15:7a:bc:a4 and ssid "free wifi for humans not a trap"
VLAN: vlan_set_name_type: SET_VLAN_NAME_TYPE_CMD name_type=2 failed: Package not installed
wlan0: interface state UNINITIALIZED->ENABLED
wlan0: AP-ENABLED 
```

* create a new network bridge:

```
sudo brctl addbr br1
```

* add the wireless interface to the new network bridge:

```
sudo brctl addif br1 wlan0
```

* set up a static network on the new bridge.  I chose a 192.168 network that's unlikely to collide with other networks in use, but you may wish to use something else (particularly if you happen to be using a network with the same addressing to configure the CubieBoard!); if so, you'll need to change it here and in the step below where you configure `dnsmasq.conf`.

```
sudo ip addr add 192.168.252.1/24 dev br0
```

* pick a MAC address for your unikernel.  In this example, I'll use the default mirage autoconfigured one, `c0:ff:ee:c0:ff:ee`.

* configure `dnsmasq`, the DHCP server, by adding the following lines to `/etc/dnsmasq.conf`:

```
interface=br1
dhcp-range=192.168.252.2,192.168.252.250,30m
dhcp-option=3,192.168.252.2
dhcp-host=c0:ff:ee:c0:ff:ee,192.168.252.2
```

This will give each client a 30 minute lease.  If you need more IPs, you can reconfigure the bridge to have a larger network and expand the dhcp range accordingly.  A unikernel with a vif having mac address c0:ff:ee:c0:ff:ee will always get the address 192.168.252.2 .  (Note that currently `simple_nat.ml` configures its addresses statically using variables set at boot time, so as of this writing the `dhcp-host` entry above is purely advisory.)

The `dhcp-host` line instructs `dnsmasq` to hand out DHCP leases that specify the default gateway as 192.168.252.2, the IP address of the NAT unikernel.

* start `dnsmasq`

```
sudo dnsmasq
```

* at this point you should be able to connect to the wifi and be given an IP, although you won't be able to browse anything (unless another device is answering to the IP address you chose for the unikernel!).

## Mirage and Unikernel Config

* download and build simple-nat:
```
opam pin add mirage-nat https://github.com/yomimono/mirage-nat
git clone https://github.com/yomimono/simple-nat
cd simple-nat
mirage configure --xen
make
```

* get a cup of your preferred beverage, respond to your e-mail, etc while the unikernel and its dependencies compile

* once that's complete, you'll have a generated `simple-nat.xl` configuration file in the `simple-nat` directory.  Edit it to contain the following line:

```
vif = [ 'mac=00:16:3e:00:00:00,bridge=br0','mac=c0:ff:ee:c0:ff:ee,bridge=br1']
```

The MAC address you set to receive a constant IP in `dnsmasq.conf` should be the same as the MAC address you set the interface with `bridge=br1` to.  The MAC address for the interface on `br0` can be anything that doesn't conflict with something else on your network.

Last, you'll need to send some network parameters to `simple-nat` when it boots, so it knows how to properly configure its network.  (For the curious, this is done through [bootvars](https://github.com/MagnusS/mirage-bootvar-xen).)  For a unikernel that boots with an "external" (on br0) IP of 192.168.2.99/24, gateway 192.168.3.1, and an "internal" IP of 192.168.252.2/24 (agreeing with what we set in `dnsmasq.conf`, boot the unikernel this way:

```
sudo xl create simple_nat.xl -c 'extra="external_ip=192.168.3.99 external_netmask=255.255.255.0 external_gateway=192.168.3.1 internal_ip=192.168.252.2 internal_netmask=255.255.255.0"'
```

(an example is included in the repository as `nat_setup.sh`)

The unikernel should then come up and begin listening for traffic on br1 to send off to br0.  If you watch outgoing traffic on the Cubie's command line via `tcpdump`, you'll see the rewritten packets going out and replies to be rewritten coming back.
