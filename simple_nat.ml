open V1_LWT
open Lwt

(* this is temporary -- waiting on merge of nojb's pr for cstruct in ipaddr,
 * see https://github.com/mirage/ocaml-ipaddr/pull/36 
 * unfortunately tcpip/lib/ipv6.ml is in this position as well *)
module V4 = struct
  type t = Ipaddr.V4.t

  let make a b c d =
let (~|) = Int32.of_int in
let (|~) = Int32.to_int in
let (&&&) x y = Int32.logand x y in
let (|||) x y = Int32.logor x y in
let (<|<) x y = Int32.shift_left x y in
let (>|>) x y = Int32.shift_right_logical x y in
let (>!)  x y = (x >|> y) &&& 0xFF_l in
let (<!)  x y = (x &&& 0xFF_l) <|< y in

    ((~| a <! 24) ||| (~| b <! 16)) ||| ((~| c <! 8) ||| (~| d <! 0))

  (* Cstruct conversion *)
  let of_cstruct_raw cs o =
    make 
      (Char.code (Cstruct.get_char cs (0 + o)))
      (Char.code (Cstruct.get_char cs (1 + o)))
      (Char.code (Cstruct.get_char cs (2 + o)))
      (Char.code (Cstruct.get_char cs (3 + o)))

  let of_cstruct_exn cs =
    let len = Cstruct.len cs in
    if len < 4 then raise (Invalid_argument (Cstruct.to_string cs));
    if len > 4 then raise (Invalid_argument (Cstruct.to_string cs));
    of_cstruct_raw cs 0

  let to_cstruct_raw i cs o =
    let (>|>) x y = Int32.shift_right_logical x y in
    let (&&&) x y = Int32.logand x y in
    let (>!)  x y = (x >|> y) &&& 0xFF_l in
    Cstruct.set_char cs (0 + o) (Char.chr (Int32.to_int (i >! 24)));
    Cstruct.set_char cs (1 + o) (Char.chr (Int32.to_int (i >! 16)));
    Cstruct.set_char cs (2 + o) (Char.chr (Int32.to_int (i >!  8)));
    Cstruct.set_char cs (3 + o) (Char.chr (Int32.to_int (i >!  0)))

  let to_cstruct i =
    let cs = Cstruct.create 4 in
    to_cstruct_raw i cs 0;
    cs
end

module V6 = struct
  let of_cstruct_raw cs o =
    let hihi = V4.of_cstruct_raw cs (o + 0) in
    let hilo = V4.of_cstruct_raw cs (o + 4) in
    let lohi = V4.of_cstruct_raw cs (o + 8) in
    let lolo = V4.of_cstruct_raw cs (o + 12) in
    Ipaddr.V6.of_int32 (hihi, hilo, lohi, lolo)

  let of_cstruct_exn cs =
    let len = Cstruct.len cs in
    if len > 16 then raise (Invalid_argument (Cstruct.to_string cs));
    if len < 16 then raise (Invalid_argument (Cstruct.to_string cs));
    of_cstruct_raw cs 0

  let to_cstruct_raw (a,b,c,d) cs o =
    V4.to_cstruct_raw a cs (0+0);
    V4.to_cstruct_raw b cs (0+4);
    V4.to_cstruct_raw c cs (0+8);
    V4.to_cstruct_raw d cs (0+12)

  let to_cstruct i =
    let cs = Cstruct.create 16 in
    to_cstruct_raw i cs 0;
    cs
end

module Main (C: V1_LWT.CONSOLE) (PRI: NETWORK) (SEC: NETWORK) = struct

  module ETH = Ethif.Make(PRI) 
  type direction = | Source | Destination

      (* construct tcp/udp modules from functors once we know whether we need
        ipv4 or ipv6 *)

  (* what level of thing does this need to take?  it needs access to ip and
     udp/tcp headers. *)
  (* at ethif level we can write our own ingestors for ipv4, ipv6; use them
     to rewrite at least ip headers, spit them out the other interface *)

  let table () = 
    let open Lookup in
    insert (empty ()) 6 (Ipaddr.of_string_exn "192.168.3.99", 6767) 
      (Ipaddr.of_string_exn "10.54.67.199", 6767)

  let listen nf push =
    (* ingest packets *)
    (* we use the PRI module type, but there's no difference between this and
       SEC; if there were, we could pass this too *)
    PRI.listen (ETH.id nf) 
      (fun frame -> return (push (Some frame)))

  let translate direction frame =
    (* TODO: attempt lookup for protocol, ips, ports to see whether nat
           req'd *)
    (* note that ethif.input doesn't have the same register-listeners-then-input
    format that tcp/udp do, so we could use it for the outer layer of parsing *)
    let retrieve_ips is_ipv6 ip_packet = 
      if is_ipv6 then
        (Ipaddr.V6 (V6.of_cstruct_exn (Wire_structs.Ipv6_wire.get_ipv6_src ip_packet)), 
        Ipaddr.V6 (V6.of_cstruct_exn (Wire_structs.Ipv6_wire.get_ipv6_dst ip_packet)))
      else
        (Ipaddr.V4 (Ipaddr.V4.of_int32 (Wire_structs.get_ipv4_src ip_packet)), 
         Ipaddr.V4 (Ipaddr.V4.of_int32 (Wire_structs.get_ipv4_dst ip_packet)))
    in
    let ip_size is_ipv6 = match is_ipv6 with 
      | false -> Wire_structs.sizeof_ipv4
      | true -> Wire_structs.Ipv6_wire.sizeof_ipv6
    in
    let retrieve_ports packet = 
      (* Cstruct.uint16, Cstruct.uint16 *)
      ((Wire_structs.get_udp_source_port packet : int), 
       (Wire_structs.get_udp_dest_port packet : int))
    in
    (* TODO: this is not the right set of parameters for a function that might
       have to do 6-to-4 translation *)
    (* also, TODO all of the 6-to-4/4-to-6 thoughts and code.  nbd. *)
    let rewrite_ips is_ipv6 (packet : Cstruct.t) (newsrc, newdst) =
      match (is_ipv6, newsrc, newdst) with
      | false, Ipaddr.V4 src, Ipaddr.V4 dst -> 
        Wire_structs.set_ipv4_src packet (Ipaddr.V4.to_int32 src);
        Wire_structs.set_ipv4_dst packet (Ipaddr.V4.to_int32 dst)
        (* TODO: every other case *)
    in
    let rewrite_ports (txlayer : Cstruct.t) (newsrc, newdst) =
      Wire_structs.set_udp_source_port txlayer newsrc;
      Wire_structs.set_udp_dest_port txlayer newdst
    in
    let ip_type = Wire_structs.get_ethernet_ethertype frame in
    let ip_packet = Cstruct.shift frame Wire_structs.sizeof_ethernet in
    match ip_type with
    | 0x0800 -> (* ipv4 *) (
      let proto = Wire_structs.get_ipv4_proto ip_packet in
      match proto with
      | 6 | 17 -> 
        let higherproto_packet = Cstruct.shift frame (ip_size false) in
        let src, dst = retrieve_ips false ip_packet in
        let sport, dport = retrieve_ports higherproto_packet in
        (* got everything; do the lookup *)
        let t = table () in
        let result = match direction with
          | Source -> Lookup.lookup t proto src sport
          | Destination -> Lookup.lookup t proto dst dport
        in
        match result, direction with
        (* TODO: just have rewrite_ips and rewrite_ports take a direction *)
        | Some (new_src, new_sport), Source ->
          rewrite_ips false ip_packet (new_src, dst); 
          rewrite_ports higherproto_packet (new_sport, dport); 
          Some frame
        | Some (new_dst, new_dport), Destination ->
          rewrite_ips false ip_packet (src, new_dst); 
          rewrite_ports higherproto_packet (sport, new_dport); 
          Some frame
        | None, _ -> None (* TODO: state tracking; inserts; check for broadcast,
                         etc *)
      | _ -> Some frame (* don't transform non-tcp/udp, just forward them *)
      (* TODO: this is obviously the wrong thing to do *)
      )
    | 0x86dd -> None (* TODO, obviously *) (* ipv6 *)
    | _ -> None (* don't forward arp or other types *)

  let shovel nf direction in_queue out_push =
    let frame_wrapper frame =
      match (translate direction frame) with
      | Some f -> return (out_push (Some f)) 
      | None -> return_unit
    in
    while_lwt true do
      Lwt_stream.next in_queue >>= frame_wrapper
    done

  let send_packets c nf out_queue =
    while_lwt true do
      lwt frame = Lwt_stream.next out_queue in
C.log c "sending a packet...\n";
      ETH.write nf frame
    done

  let start c pri sec =
  (* TODO: for now we assume that any packet we receive on an interface needs to
    go out the other interface; in reality, some would be bound for us,
     unroutable, broadcast, etc *)

  (* TODO: should these be parameterized by associated network somehow? *)
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

    lwt nf1 = or_error c "primary interface" ETH.connect pri in
    lwt nf2 = or_error c "secondary interface" ETH.connect sec in
    Lwt.choose [
    (listen nf1 pri_in_push);
    (listen nf2 sec_in_push); 
    (shovel nf1 Destination pri_in_queue sec_out_push);
    (shovel nf2 Source sec_in_queue pri_out_push);
    (send_packets c nf1 pri_out_queue); 
    (send_packets c nf2 sec_out_queue)
  ]

end
