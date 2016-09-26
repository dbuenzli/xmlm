(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% version %%VERSION%%
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
  ()

let test () =
  Printexc.record_backtrace true;
  decoder_strip_atts ();
  log "All tests succeeded.\n"

let () = if not (!Sys.interactive) then test ()

(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli
   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions
   are met:

   1. Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

   2. Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

   3. Neither the name of Daniel C. Bünzli nor the names of
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  ---------------------------------------------------------------------------*)
