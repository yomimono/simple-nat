open Mirage

let arg name opt =
  Key.create name @@ Key.Arg.required Key.Arg.string (Key.Arg.info [opt])

let external_ip = arg "external_ip" "external-ip"
let internal_ip = arg "internal_ip" "internal-ip"
let internal_netmask = arg "internal_netmask" "internal-netmask"
let external_gateway = arg "external_gateway" "external-gateway"
let external_netmask = arg "external_netmask" "external-netmask"

let keys = Key.[
  abstract external_ip;
  abstract internal_ip;
  abstract external_netmask;
  abstract internal_netmask;
  abstract external_gateway;
]

let main = foreign "Simple_nat.Main" ~keys (console @-> clock @-> time @-> network @-> network @-> job)

let primary_netif = (netif "0")
let secondary_netif = (netif "1") (* netif actually needs an integer, shoved
into a string, which maps to a device ID number assigned by Xen, to do anything
helpful when xen is the target.  Stuff that can't be turned into an int
is silently dropped in that case and we just get the first Xen network iface. *)
(* 
let tracing = mprof_trace ~size:100000 () *)

let () =
  add_to_opam_packages ["mirage-nat";"tcpip";"mirage-profile"];
  add_to_ocamlfind_libraries ["mirage-nat";"mirage-nat.hashtable";"tcpip.arpv4";
                              "tcpip.ethif";"tcpip.ipv4";"mirage-profile"];
  register "simple-nat" (* ~tracing *) [
    main $ default_console $ default_clock $ default_time $ primary_netif $ secondary_netif
  ]
