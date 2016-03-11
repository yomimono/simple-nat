open Mirage

let main = foreign "Simple_nat.Main" (console @-> clock @-> time @-> network @-> network @-> job)

let primary_netif = (netif "0")
let secondary_netif = (netif "1") (* netif actually needs an integer, shoved
into a string, which maps to a device ID number assigned by Xen, to do anything
helpful when xen is the target.  Stuff that can't be turned into an int
is silently dropped in that case and we just get the first Xen network iface. *)
(* 
let tracing = mprof_trace ~size:100000 () *)

let () =
  add_to_opam_packages ["mirage-nat";"re";"tcpip";"mirage-profile"];
  add_to_ocamlfind_libraries ["mirage-nat";"mirage-nat.hashtable";"re.str";"tcpip.arpv4";
                              "tcpip.ethif";"tcpip.ipv4";"mirage-profile"];
  register "simple-nat" (* ~tracing *) [
    main $ default_console $ default_clock $ default_time $ primary_netif $ secondary_netif
  ]
