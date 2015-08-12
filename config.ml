open Mirage

let main = foreign "Simple_nat.Main" (console @-> random @-> clock @-> network @-> network @->
                                      http @-> job)

let console = default_console

(* 0 is usually the bridge with other stuff on it *)
(* so the "first" vif offered to us will be a "management" interface *)
let stack = direct_stackv4_with_dhcp console (netif "0")
let port = `Tcp (`Port 80)
let http = http_server (conduit_direct stack)

(* netif actually needs an integer, shoved
into a string, which maps to a device ID number assigned by Xen, to do anything
helpful when xen is the target.  Stuff that can't be turned into an int
is silently dropped in that case and we just get the first Xen network iface. *)

let primary_netif = (netif "1")
let secondary_netif = (netif "2")

let fs = crunch "static"

let () =
  add_to_opam_packages
    ["mirage-nat";"irmin";"re";"tcpip";"mirage-profile";"irmin-arp"];
  add_to_ocamlfind_libraries ["mirage-nat";"re.str";"irmin.http";
                              "irmin-arp";"tcpip.ethif";"tcpip.ipv4";"mirage-profile"];
  register "simple-nat" [
    main $ console $ default_random $ default_clock $ primary_netif $ secondary_netif $ http
  ]
