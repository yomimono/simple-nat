open V1_LWT
open Lwt
open Nat_rewrite

module Main (C: CONSOLE) (PRI: NETWORK) (SEC: NETWORK) = struct

  module ETH = Ethif.Make(PRI)
  module I = Ipv4.Make(ETH)
  type direction = Nat_rewrite.direction

  let listen nf i push =
    (* ingest packets *)
    PRI.listen nf
      (fun frame ->
         match (Wire_structs.get_ethernet_ethertype frame) with
         | 0x0806 -> I.input_arpv4 i frame
         | _ -> return (push (Some frame)))

  let allow_nat_traffic table frame ip =
    let rec stubborn_insert table frame ip port = match port with
      (* TODO: in the unlikely event that no port is available, this
         function will never terminate *)
            (* TODO: lookup (or someone, maybe tcpip!)
               should have a facility for choosing a random unused
               source port *)
      | n when n < 1024 ->
        stubborn_insert table frame ip (Random.int 65535)
      | n ->
        match Nat_rewrite.make_nat_entry table frame ip n with
        | Ok t -> Some t
        | Unparseable -> None
        | Overlap -> stubborn_insert table frame ip (Random.int 65535)
    in
    (* TODO: connection tracking logic *)
    stubborn_insert table frame ip (Random.int 65535)

  (* other_ip means the IP held by the NAT device on the interface which *isn't*
     the one that received this traffic *)
  let allow_rewrite_traffic table frame other_ip client_ip fwd_port =
    let rec stubborn_insert table frame other_ip client_ip fwd_port xl_port =
      match xl_port with
      | n when n < 1024 -> stubborn_insert table frame other_ip client_ip
                             fwd_port (Random.int 65535)
      | n ->
        match Nat_rewrite.make_redirect_entry table frame (other_ip, n)
                (client_ip, fwd_port)
        with
        | Ok t -> Some t
        | Unparseable -> None
        | Overlap -> stubborn_insert table frame other_ip client_ip
                       fwd_port (Random.int 65535)
    in
    stubborn_insert table frame other_ip client_ip fwd_port (Random.int 65535)

  let shovel external_ip internal_ip (internal_client, fwd_dport)
      nat_table (direction : direction)
      in_queue out_push =
    let rec frame_wrapper frame =
      (* typical NAT logic: traffic from the internal "trusted" interface gets
         new mappings by default; traffic from other interfaces gets dropped if
         no mapping exists (which it doesn't, since we already checked) *)
      match direction, (Nat_rewrite.translate nat_table direction frame) with
      | Destination, None ->
        (
        (* if this isn't return traffic from an outgoing request, check to see
           whether it's traffic we know we should forward on to internal_client
           because of preconfigured port forward mappings
        *)
          (* known because we already matched on Direction = Destination *)
          let (my_ip, other_ip) = external_ip, internal_ip in 
          match Nat_rewrite.layers frame with
          | None -> return_unit (* unparseable packet; drop it *)
          | Some (ethernet_layer, ip_layer, transport_layer) ->
            let (frame_src, frame_dst) = addresses_of_ip ip_layer in
            let (frame_sport, frame_dport) = ports_of_transport transport_layer in
              if (frame_dst = my_ip && frame_dport = fwd_dport) then
                (* rewrite traffic to come from our other interface and go to the
                   preconfigured client IP *)
                match allow_rewrite_traffic nat_table frame other_ip internal_client
                        fwd_dport with
                | None -> return_unit
                | Some nat_table ->
                  match Nat_rewrite.translate nat_table direction frame with
                  | None -> return_unit
                  | Some f -> return (out_push (Some f))
              else return_unit
        )
      | _, Some f ->
        return (out_push (Some f))
      | Source, None ->
        (* mutate nat_table to include entries for the frame *)
        match allow_nat_traffic nat_table frame internal_ip with
        | Some t ->
          (* try rewriting again; we should now have an entry for this packet *)
          frame_wrapper frame
        | None ->
          (* this frame is hopeless! *)
          return_unit
    in
    while_lwt true do
      Lwt_stream.next in_queue >>= frame_wrapper
    done

let send_packets c nf i out_queue =
  while_lwt true do
    lwt frame = Lwt_stream.next out_queue in

    let new_smac = Macaddr.to_bytes (PRI.mac nf) in
    Wire_structs.set_ethernet_src new_smac 0 frame;
    let ip_layer = Cstruct.shift frame (Wire_structs.sizeof_ethernet) in
    let ipv4_frame_size = (Wire_structs.Ipv4_wire.get_ipv4_hlen_version ip_layer land 0x0f) * 4 in
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
    let () = match Wire_structs.Ipv4_wire.get_ipv4_proto ip_layer with
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

  (* get network configuration from bootvars *)
  Bootvar.create >>= fun bootvar ->
  let try_bootvar key = Ipaddr.V4.of_string_exn (Bootvar.get bootvar key) in
  let internal_ip = try_bootvar "internal_ip" in
  let internal_netmask = try_bootvar "internal_netmask" in
  let external_ip = try_bootvar "external_ip" in
  let external_netmask = try_bootvar "external_netmask" in
  let external_gateway = try_bootvar "external_gateway" in
  let internal_client = try_bootvar "internal_client" in
  let intercept_port = int_of_string (Bootvar.get bootvar "dest_port") in

  (* initialize interfaces *)
  lwt nf1 = or_error c "primary interface" ETH.connect pri in
  lwt nf2 = or_error c "secondary interface" ETH.connect sec in

  (* set up ipv4 on interfaces so ARP will be answered *)
  lwt ext_i = or_error c "ip for primary interface" I.connect nf1 in
  lwt int_i = or_error c "ip for secondary interface" I.connect nf2 in
  I.set_ip ext_i external_ip >>= fun () ->
  I.set_ip_netmask ext_i external_netmask >>= fun () ->
  I.set_ip int_i internal_ip >>= fun () ->
  I.set_ip_netmask int_i internal_netmask >>= fun () ->
  I.set_ip_gateways ext_i [ external_gateway ] >>= fun () -> ();

  (* TODO: provide hooks for updates to/dump of this *)
  let table () = Nat_lookup.empty ()
  in

  let nat_t = table () in
  let nat = shovel in

  Lwt.choose [
    (* packet intake *)
    (listen pri ext_i pri_in_push);
    (listen sec int_i sec_in_push);

    (* TODO: ICMP, at least on our own behalf *)

    (* address translation *)
    (* for packets received on the first interface (xenbr0/br0 in examples,
       which is an "external" world-facing interface),
       rewrite destination addresses/ports before sending packet out the second
       interface *)
    (nat (V4 external_ip) (V4 internal_ip)
       ((Ipaddr.V4 internal_client), intercept_port) nat_t
       Destination pri_in_queue sec_out_push);

    (* for packets received on xenbr1 ("internal"), rewrite source address/port
       before sending packets out the primary interface *)
    (nat (V4 external_ip) (V4 internal_ip)
       ((Ipaddr.V4 internal_client), intercept_port) nat_t
       Source sec_in_queue pri_out_push);

    (* packet output *)
    (send_packets c pri ext_i pri_out_queue);
    (send_packets c sec int_i sec_out_queue)
  ]

end
