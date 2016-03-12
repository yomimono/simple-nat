open V1_LWT
open Lwt

module Main (C: CONSOLE) (Clock: V1.CLOCK) (Time: V1_LWT.TIME)
    (PRI: NETWORK) (SEC: NETWORK) = struct

  module Nat_clock = struct
    let now () = Clock.time () |> Int64.of_float
  end

  module Nat_rewrite = Mirage_nat_hashtable.Make(Nat_clock)(Time)

  module ETH = Ethif.Raw(PRI)
  module A = Arpv4.Make(ETH)(Clock)(Time)
  module I = Ipv4.Make(ETH)(A)

  type direction = | Source | Destination

  let listen netif ethif a push =
    (* ingest packets *)
    PRI.listen netif
      (ETH.input ethif
        ~arpv4:(A.input a)
        ~ipv4:(fun frame -> push (Some frame); return_unit)
        ~ipv6:(fun _ -> return_unit)
      )

  let allow_nat_traffic table frame ip =
    let rec stubborn_insert table frame ip port = match port with
      (* TODO: in the unlikely event that no port is available, this
         function will never terminate (this is really a tcpip todo) *)
      | n when n < 1024 ->
        stubborn_insert table frame ip (Random.int 65535)
      | n ->
        let open Nat_rewrite in
        add_nat table frame (ip, n) >>= function
        | Ok -> Lwt.return (Some ())
        | Unparseable -> Lwt.return None
        | Overlap -> stubborn_insert table frame ip (Random.int 65535)
    in
    (* TODO: connection tracking logic *)
    stubborn_insert table frame ip (Random.int 65535)

  let nat external_ip internal_ip nat_table (direction : direction)
      in_queue out_push =
    let rec aux ip =
      let open Mirage_nat in
      (* typical NAT logic: traffic from the internal "trusted" interface gets
         new mappings by default; traffic from other interfaces gets dropped if
         no mapping exists (which it doesn't, since we already checked) *)
      Nat_rewrite.translate nat_table ip >>= fun f ->
      match direction, f with
      | Destination, Untranslated -> 
        Lwt.return_unit (* nothing in the table, drop it *)
      | _, Translated ->
        return (out_push (Some (Nat_decompose.finalize_packet ip)))
      | Source, Untranslated ->
        (* mutate nat_table to include entries for the frame *)
        allow_nat_traffic nat_table frame external_ip >>= function
        | Some () ->
          (* try rewriting again; we should now have an entry for this packet *)
          aux ip
        | None ->
          (* this frame is hopeless! *)
          return_unit
    in
    while_lwt true do
      Lwt_stream.next in_queue >>= aux
    done

let send_packets c nf i (out_queue : Cstruct.t Lwt_stream.t) =
  while_lwt true do
    lwt frame = Lwt_stream.next out_queue in

    match Nat_decompose.layers frame with
    | None -> raise (Invalid_argument "NAT transformation rendered packet unparseable")
    | Some ip_packet ->
      let dst = Wire_structs.Ipv4_wire.get_ipv4_dst in
      A.query a dst >>= function
      | None -> return_unit
      | Some dst ->
      Eth.output ~dst ~proto:`IPv4 ip_packet
  done

let start c _clock _time pri sec =

let (pri_in_queue, pri_in_push) = Lwt_stream.create () in
let (pri_out_queue, pri_out_push) = Lwt_stream.create () in
let (sec_in_queue, sec_in_push) = Lwt_stream.create () in
let (sec_out_queue, sec_out_push) = Lwt_stream.create () in

(* or_error brazenly stolen from netif-forward *)
let or_error c name fn t =
  fn t
  >>= function
  | `Error e -> fail (Failure ("error starting " ^ name))
  | `Ok t -> C.log_s c (Printf.sprintf "%s connected." name) >>
    return t
in

(* get network configuration from bootvars *)
let fix = Ipaddr.V4.of_string_exn in
let internal_ip = fix @@ Key_gen.internal_ip () in
let internal_netmask = fix @@ Key_gen.internal_netmask () in
let external_ip = fix @@ Key_gen.external_ip () in
let external_netmask = fix @@ Key_gen.external_netmask () in
let external_gateway = fix @@ Key_gen.external_gateway () in

(* initialize interfaces *)
lwt ethif1 = or_error c "primary interface" ETH.connect pri in
lwt ethif2 = or_error c "secondary interface" ETH.connect sec in

lwt arp1 = or_error c "primary arp" A.connect ethif1 in
lwt arp2 = or_error c "primary arp" A.connect ethif2 in

(* set up ipv4 on interfaces so ARP will be answered *)
lwt ext_i = or_error c "ip for primary interface" (I.connect ethif1) arp1 in
lwt int_i = or_error c "ip for secondary interface" (I.connect ethif2) arp2 in
I.set_ip ext_i external_ip >>= fun () ->
I.set_ip_netmask ext_i external_netmask >>= fun () ->
I.set_ip int_i internal_ip >>= fun () ->
I.set_ip_netmask int_i internal_netmask >>= fun () ->
I.set_ip_gateways ext_i [ external_gateway ] >>= fun () -> ();

(* TODO: provide hooks for updates to/dump of this *)
Nat_rewrite.empty () >>= fun nat_t ->

  Lwt.choose [
    (* packet intake *)
    (listen pri ethif1 arp1 pri_in_push);
    (listen sec ethif2 arp2 sec_in_push);

    (* TODO: ICMP, at least on our own behalf *)

    (* address translation *)

    (* for packets received on xenbr1 ("internal"), rewrite source address/port
       before sending packets out the primary interface *)
    (nat (Ipaddr.V4 external_ip) (Ipaddr.V4 internal_ip) nat_t Source sec_in_queue pri_out_push);

    (* for packets received on the first interface (xenbr0/br0 in examples,
       which is an "external" world-facing interface),
       rewrite destination addresses/ports before sending packet out the second
       interface *)
    (nat (Ipaddr.V4 external_ip) (Ipaddr.V4 internal_ip) nat_t Destination pri_in_queue sec_out_push);

    (* packet output *)
    (send_packets c pri ext_i pri_out_queue);
    (send_packets c sec int_i sec_out_queue)
  ]

end
