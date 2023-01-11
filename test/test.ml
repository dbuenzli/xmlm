(*---------------------------------------------------------------------------
   Copyright (c) 2014 The xmlm programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

let str = Format.sprintf
let log f = Format.printf (f ^^ "@?")
let fail fmt =
  let fail _ = failwith (Format.flush_str_formatter ()) in
  Format.kfprintf fail Format.str_formatter fmt

(* We should add mode more coverage here see e.g. what is done in jsonm. *)

let test_decode fnd exp =
  if fnd <> exp
  then fail "found: %a expected: %a" Xmlm.pp_signal fnd Xmlm.pp_signal exp

let test_seq ?enc ?strip ?ns ?entity ?dtd src seq =
  let d = Xmlm.make_input ?enc ?strip ?ns ?entity (`String (0, src)) in
  let rec loop d = function [] -> ()
  | v :: vs -> test_decode (Xmlm.input d) v; loop d vs
  in
  try
    let seq = match dtd with None -> `Dtd None :: seq | Some d -> d :: seq in
    loop d seq;
    if not (Xmlm.eoi d) then fail "Expected end of input"
  with Xmlm.Error ((l,c), e) ->
    fail "error:%d:%d: %s" l c (Xmlm.error_message e)

let name ?(ns = "") n = (ns, n)
let att ?ns n v = name ?ns n, v
let tag ?(atts = []) ?ns n = (name ?ns n), atts
let el ?atts ?ns n content =
  (`El_start (tag ?atts ?ns n)) :: List.flatten content @ [`El_end]

let decoder_strip_atts () =
  log "Decoder attribute stripping.\n";
  let test_attv v pv =
    test_seq (str "<e a ='%s'></e>" v) (el "e" ~atts:[att "a" pv] [])
  in
  test_attv "  bla bli\n\n blo " "bla bli blo";
  let test_iso_8859_15 v pv =
    test_seq ~enc:(Some `ISO_8859_15) (str "<e>%s</e>" v) (el "e" [[`Data pv]])
  in
  List.iter
    (fun (v, pv) -> test_iso_8859_15 v pv)
    [
      ("\065", "\u{0041}"); (* A *)
      ("\164", "\u{20AC}"); (* € *)
      ("\166", "\u{0160}"); (* Š *)
      ("\168", "\u{0161}"); (* š *)
      ("\180", "\u{017D}"); (* Ž *)
      ("\184", "\u{017E}"); (* ž *)
      ("\188", "\u{0152}"); (* Œ *)
      ("\189", "\u{0153}"); (* œ *)
      ("\190", "\u{0178}"); (* Ÿ *)
    ];
  ()

let test () =
  Printexc.record_backtrace true;
  decoder_strip_atts ();
  log "All tests succeeded.\n"

let () = if not (!Sys.interactive) then test ()

(*---------------------------------------------------------------------------
   Copyright (c) 2014 The xmlm programmers

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
