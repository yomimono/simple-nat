open Mirage

let main = foreign "Simple_nat.Main" (console @-> network @-> network @-> job)

let primary_netif = (netif "0")
let secondary_netif = (netif "1") (* netif actually needs an integer, shoved
into a string, which maps to a device ID number assigned by Xen, to do anything 
helpful when xen is the target.  Stuff that can't be turned into an int
is silently dropped in that case and we just get the first Xen network iface. *)

(* 
let tracing = mprof_trace ~size:100000 () *)

let () = 
  add_to_opam_packages ["mirage-nat";"tcpip";"re";"mirage-profile"];
  add_to_ocamlfind_libraries ["mirage-nat";"re";"re.str";
                              "tcpip.ethif";"tcpip.ipv4";"mirage-profile"];
  register "simple_nat" (* ~tracing *) [
    main $ default_console $ primary_netif $ secondary_netif 
  ]
