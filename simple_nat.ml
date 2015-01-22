open V1_LWT
open Lwt

module Main (C: V1_LWT.CONSOLE) (PRI: NETWORK) (SEC: NETWORK) = struct

  module ETH = Ethif.Make(PRI) (*
  module IPV4 = Ipv4.Make(ETH)
  module IPV6 = Ipv6.Make(ETH) *)
      (* construct tcp/udp modules from functors once we know whether we need
        ipv4 or ipv6 *)

  (* what level of thing does this need to take?  it needs access to ip and
     udp/tcp headers. *)
  (* at ethif level we can write our own ingestors for ipv4, ipv6; use them
     to rewrite at least ip headers, spit them out the other interface *)
  let table = 
    let open Lookup in
    insert (empty ()) 6 (Ipaddr.of_string_exn "192.168.3.1", 6767) 
      (Ipaddr.of_string_exn "10.1.5.6", 10111)

  let listen nf push =
    (* ingest packets *)
    (* we use the PRI module type, but there's no difference between this and
       SEC; if there were, we could pass this too *)
    PRI.listen (ETH.id nf) 
      (fun frame -> return (push (Some frame)))

  let translate frame =
    (* TODO: attempt lookup for protocol, ips, ports to see whether nat
           req'd *)

    (* let ipv4 packet = ... uh *)
      (* packets sent here will be of type ipv4 & have headers filled in *)
      (* ...or will they?  actually they just have the ethernet frame header
        chopped off; the ipv4/ipv6 functions themselves have to deal with the
         rest *)
      (* seems obnoxious to have to dig around in there ourselves, and yet what
      we have to do also obnoxiously crosses the boundary between ip and the
         protocol above. *)
      (* can we call listen with an ipv4 processor that's actually just the
         tcpip library's own parser/forwarder? *)

    Some frame

  let shovel nf in_queue out_push =
    while_lwt true do
      lwt _ = Lwt_stream.next in_queue >>= 
      fun frame -> return (out_push (translate frame)) in
      return_unit
    done

  let send_packets nf out_queue =
    while_lwt true do
      lwt frame = Lwt_stream.next out_queue in
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
    (shovel nf1 pri_in_queue sec_out_push);
    (shovel nf2 sec_in_queue pri_out_push);
    (send_packets nf1 pri_out_queue);
    (send_packets nf2 sec_out_queue)
  ]

end
