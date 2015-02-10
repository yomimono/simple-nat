open V1_LWT
open Lwt

module Main (C: CONSOLE) (PRI: NETWORK) (SEC: NETWORK) = struct

  module ETH = Ethif.Make(PRI) 
  module I = Ipv4.Make(ETH)
  type direction = | Source | Destination

  (* TODO: should probably use config.ml stack configuration stuff instead *)
  (* TODO: icmp crashes the unikernel, which is not optimal *)
  let external_ip = (Ipaddr.of_string_exn "192.168.3.99") 
  let external_netmask = (Ipaddr.V4.of_string_exn "255.255.255.0")
  let internal_ip = (Ipaddr.V4.of_string_exn "10.0.0.1")
  let internal_netmask = (Ipaddr.V4.of_string_exn "255.255.255.0")
  let external_gateway = (Ipaddr.V4.of_string_exn "192.168.3.1") 

  (* what level of thing does this need to take?  it needs access to ip and
     udp/tcp headers. *)
  (* at ethif level we can write our own ingestors for ipv4, ipv6; use them
     to rewrite at least ip headers, spit them out the other interface *)

  let table () = 
    let open Lookup in
    (* TODO: rewrite as a bind *)
    match insert (empty ()) 6 (Ipaddr.of_string_exn "10.0.0.2", 80) 
            (Ipaddr.of_string_exn "192.168.3.1", 52966)(external_ip, 9999) with
    | None -> raise (Failure "Couldn't create hardcoded NAT table")
    | Some t -> 
      match insert t 17 (Ipaddr.of_string_exn "10.0.0.2", 53)
              (Ipaddr.of_string_exn "192.168.3.1", 52966)
              (external_ip, 9999) with
      | None -> raise (Failure "Couldn't create hardcoded NAT table")
      | Some t -> t

  let listen nf i push =
    (* ingest packets *)
    PRI.listen (ETH.id nf) 
      (fun frame -> 
         match (Wire_structs.get_ethernet_ethertype frame) with
         | 0x0806 -> I.input_arpv4 i frame
         | _ -> return (push (Some frame)))

  let shovel c nf table direction in_queue out_push =
    let frame_wrapper frame =
      match (Rewrite.translate table direction frame) with
      | Some f -> 
        MProf.Counter.increase c 1;
        return (out_push (Some f)) 
      | None -> 
        return_unit
    in
    while_lwt true do
      Lwt_stream.next in_queue >>= frame_wrapper
    done

let send_packets c nf i out_queue =
  while_lwt true do
    lwt frame = Lwt_stream.next out_queue in
    (* TODO: we're assuming this is ipv4 which is obviously not necessarily correct
    *)

    let new_smac = Macaddr.to_bytes (ETH.mac nf) in
    Wire_structs.set_ethernet_src new_smac 0 frame;
    let ip_layer = Cstruct.shift frame (Wire_structs.sizeof_ethernet) in
    let ipv4_frame_size = (Wire_structs.get_ipv4_hlen_version ip_layer land 0x0f) * 4 in
    let higherlevel_data = 
      Cstruct.sub frame (Wire_structs.sizeof_ethernet + ipv4_frame_size)
      (Cstruct.len frame - (Wire_structs.sizeof_ethernet + ipv4_frame_size))
    in
    let just_headers = Cstruct.sub frame 0 (Wire_structs.sizeof_ethernet +
                                            ipv4_frame_size) in
    let fix_checksum set_checksum ip_layer higherlevel_data =
      (* reset checksum to 0 for recalculation *)
      set_checksum higherlevel_data 0;
      let actual_checksum = I.checksum just_headers (higherlevel_data ::
                                                     []) in
      set_checksum higherlevel_data actual_checksum
    in
    let () = match Wire_structs.get_ipv4_proto ip_layer with
    | 17 -> 
      fix_checksum Wire_structs.set_udp_checksum ip_layer higherlevel_data
    | 6 -> 
      fix_checksum Wire_structs.Tcp_wire.set_tcp_checksum ip_layer higherlevel_data
    | _ -> ()
    in
    I.writev i just_headers [ higherlevel_data ] >>= fun () -> return_unit
  done

let start c pri sec =

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

  let to_v4_exn ip =
    match (Ipaddr.to_v4 ip) with
    | None -> raise (Failure ("Attempted to convert an incompatible IP to IPv4: "
                             ^ (Ipaddr.to_string ip)))
    | Some i -> i
  in

  (* initialize interfaces *)
  lwt nf1 = or_error c "primary interface" ETH.connect pri in
  lwt nf2 = or_error c "secondary interface" ETH.connect sec in

  (* set up ipv4 on interfaces so ARP will be answered *)
  lwt ext_i = or_error c "ip for primary interface" I.connect nf1 in
lwt int_i = or_error c "ip for secondary interface" I.connect nf2 in
  I.set_ip ext_i (to_v4_exn external_ip) >>= fun () ->
  I.set_ip_netmask ext_i external_netmask >>= fun () ->
  I.set_ip int_i internal_ip >>= fun () ->
  I.set_ip_netmask int_i internal_netmask >>= fun () ->
  I.set_ip_gateways ext_i [ external_gateway ] >>= fun () -> ();

  (* initialize hardwired lookup table *)
let t = table () in

let translated_packets = MProf.Counter.make "forwarded packets" in
 
  Lwt.choose [
    (* packet intake *)
    (listen nf1 ext_i pri_in_push);
    (listen nf2 int_i sec_in_push); 
    
    (* address translation *)
    (shovel translated_packets nf1 t Destination pri_in_queue sec_out_push);
    (shovel translated_packets nf2 t Source sec_in_queue pri_out_push);

    (* packet output *)
    (send_packets c nf1 ext_i pri_out_queue); 
    (send_packets c nf2 int_i sec_out_queue)
  ]

end
