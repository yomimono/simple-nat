open Mirage

let main = foreign "Simple_nat.Main" (console @-> network @-> job)

let primary_netif = (netif "0")

let tracing = mprof_trace ~size:100000 ()

let () = 
  add_to_opam_packages ["ocaml-nat";"tcpip";"mirage-profile"];
  add_to_ocamlfind_libraries ["mirage_nat";
                              "tcpip.ethif";"tcpip.ipv4";"mirage-profile"];
  register "simple_nat" ~tracing [
    main $ default_console $ primary_netif 
  ]
