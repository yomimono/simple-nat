open Mirage

let main = foreign "Simple_nat.Main" (console @-> network @-> network @-> job)

let primary_netif = (netif "0")
let secondary_netif = (netif "1") (* netif actually needs an integer, shoved
into a string, which maps to a device ID number assigned by Xen, to do anything 
helpful when xen is the target.  Stuff that can't be turned into an int
is silently dropped in that case and we just get the first Xen network iface. *)

let primary_stack = direct_stackv4_with_dhcp default_console primary_netif
let secondary_stack = direct_stackv4_with_dhcp default_console secondary_netif

let () = 
  add_to_opam_packages ["ocaml-nat";"tcpip"];
  add_to_ocamlfind_libraries ["mirage_nat"; "tcpip.ethif" ];
  register "simple_nat" [
    main $ default_console $ primary_netif $ secondary_netif 
  ]
