(*
 * Copyright (c) 2014-2015 Magnus Skjegstad <magnus@v0.no>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)
open V1_LWT
open Lwt
open Ipaddr
open String
open Re

(* Based on mirage-skeleton/xen/static_website+ip code for reading boot parameters *)
type t = { cmd_line : string;
           parameters : (string * string) list }

(* read boot parameter line and store in assoc list - expected format is "key1=val1 key2=val2" *)
let create = 
  OS.Xs.make () >>= fun client ->
  OS.Xs.(immediate client (fun x -> read x "vm")) >>= fun vm ->
  OS.Xs.(immediate client (fun x -> read x (vm^"/image/cmdline"))) >>= fun cmd_line ->
  (*let cmd_line = OS.Start_info.((get ()).cmd_line) in*)
  Printf.printf "OS cmd_line is %s\n" cmd_line;
  let entries = Re_str.(split (regexp_string " ") cmd_line) in
  let vartuples =
    List.map (fun x ->
        match Re_str.(split (regexp_string "=") x) with 
        | [a;b] -> Printf.printf "%s=%s\n" a b ; (a,b)
        | _ -> raise (Failure "malformed boot parameters")) entries
  in
  Lwt.return { cmd_line = cmd_line; parameters = vartuples}

(* get boot parameter *)
let get t parameter = 
  try 
    List.assoc parameter t.parameters
  with
    Not_found -> Printf.printf "Boot parameter %s not found\n" parameter; raise Not_found

