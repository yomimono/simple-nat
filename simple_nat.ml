open V1_LWT
open Lwt

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

  let shovel c nf table direction in_queue out_push =
    let frame_wrapper frame =
      match (Rewrite.translate table direction frame) with
      | Some f -> 
        MProf.Counter.increase c 1;
        return (out_push (Some f)) 
      | None -> return_unit
    in
    while_lwt true do
      Lwt_stream.next in_queue >>= frame_wrapper
    done

let send_packets c nf out_queue =
    while_lwt true do
  lwt frame = Lwt_stream.next out_queue in
      ETH.write nf frame
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

  (* initialize interfaces *)
  lwt nf1 = or_error c "primary interface" ETH.connect pri in
  lwt nf2 = or_error c "secondary interface" ETH.connect sec in

  (* initialize hardwired lookup table *)
let t = table () in

let translated_packets = MProf.Counter.make "forwarded packets" in
 
  Lwt.choose [
    (* packet intake *)
    (listen nf1 pri_in_push);
    (listen nf2 sec_in_push); 
    
    (* address translation *)
    (shovel translated_packets nf1 t Destination pri_in_queue sec_out_push);
    (shovel translated_packets nf2 t Source sec_in_queue pri_out_push);

    (* packet output *)
    (send_packets c nf1 pri_out_queue); 
    (send_packets c nf2 sec_out_queue)
  ]

end
