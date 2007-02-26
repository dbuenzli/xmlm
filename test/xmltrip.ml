(*----------------------------------------------------------------------------
   Copyright (c) %%COPYRIGHTYEAR%%, Daniel C. Bünzli. All rights reserved.
   Distributed under a BSD license, see license at the end of the file.
  ----------------------------------------------------------------------------*)

let str = Printf.sprintf
let exec = Filename.basename Sys.executable_name
let pr_err s = Printf.eprintf "%s:%s\n" exec s
let apply f x ~finally y = 
  let result = try f x with exn -> finally y; raise exn in
  finally y;
  result
        
let xml_parse tree enc strip entity ic () =                    (* parse only *)
  let i = Xmlm.input_of_channel ic in
  if tree then ignore (Xmlm.input_tree ~enc ~strip ~entity () i) else
  Xmlm.input ~enc ~strip ~entity () i
  
let xml_outline tree enc strip entity ic oc =               (* ascii outline *)
  let pr s = Printf.fprintf oc s in
  let pr_dtd dtd = match dtd with Some s -> pr "+-DTD %S\n" s | _ -> () in
  let pr_depth d = for k = 1 to d do pr "| " done in
  let pr_data d data = pr_depth d; pr "%S\n" data in
  let pr_name c (p, l) =  if p <> "" then pr "%s:%s" p l else pr "%s" l in
  let pr_att d (n, v) = pr_depth (d + 1); pr "* %a = %S\n" pr_name n v in
  let pr_tag d (n, atts) = 
    pr_depth d; pr "+-%a\n" pr_name n; List.iter (pr_att d) atts 
  in
  let i = Xmlm.input_of_channel ic in
  if tree then 
    let rec pr_tree d = function
      | (n :: next) :: path -> 
	  begin match n with
	  | `El ((), tag, childs) -> 
	      pr_tag d tag; pr_tree (d+1) (childs :: next :: path)
	  | `D data -> 
	      pr_data d data; pr_tree d (next :: path)
	  end
      | [] :: [] -> ()
      | [] :: path -> pr_tree (d - 1) path
      | [] -> assert false
    in
    match Xmlm.input_tree ~enc ~strip ~entity ~prolog:pr_dtd () i with
    | Some t -> pr_tree 0 [ [ t ] ]; flush oc
    | None -> assert false
  else
    let s tag depth = pr_tag depth tag; depth + 1 in
    let d data depth = pr_data depth data; depth in
    let e _ depth = depth - 1 in
    ignore (Xmlm.input ~enc ~strip ~entity ~prolog:pr_dtd ~s ~d ~e 0 i);
    flush oc

let xml_xml indent tree enc strip entity ic oc =                (* xml trip *)
  let nl = (indent = None) in
  let i = Xmlm.input_of_channel ic in
  let o = Xmlm.output_of_channel ~indent oc in
  let prolog = Xmlm.output_prolog o in
  if tree then 
    match Xmlm.input_tree ~enc ~strip ~entity ~prolog () i with
    | Some t -> Xmlm.output_tree o t; Xmlm.output_finish ~nl o 
    | None -> assert false
  else
    let d data _ = Xmlm.output_signal o (`D data) in 
    let s tag _ = Xmlm.output_signal o (`S tag) in
    let e _ _ = Xmlm.output_signal o `E in
    Xmlm.input ~enc ~strip ~entity ~prolog ~d ~s ~e () i;
    Xmlm.output_finish ~nl o

let with_inf f inf v = 
  try
    let ic = if inf <> "" then open_in_bin inf else stdin in 
    let close ic = if inf <> "" then close_in ic else () in 
    apply (f ic) v ~finally:close ic
  with
  | Sys_error e -> pr_err (str " %s" e)
  | Xmlm.Error ((l,c), e) -> 
      pr_err (str "%s:%d:%d: %s" inf l c (Xmlm.error_message e))
	
let with_outf f ic outf = 
  try 
    let oc = if outf <> "" then open_out_bin outf else stdout in 
    let close oc = if outf <> "" then close_out oc else () in 
    apply (f ic) oc ~finally:close oc
  with
  | Sys_error e -> pr_err (str " %s" e)
        
let process tree enc strip ename parse_only outline indent suffix files = 
  let entity = if ename then fun x -> Some x else fun x -> None in 
  let f = 
    if parse_only then 
      fun inf -> with_inf (xml_parse tree enc strip entity) inf ()
    else 
      let outf inf =  
	if inf = "" || suffix = "" then "" (* stdout *) else 
	str "%s.%s" inf suffix 
      in
      let f = if outline then xml_outline else (xml_xml indent) in
      fun inf -> with_inf (with_outf (f tree enc strip entity)) inf (outf inf)
  in
  List.iter f files

let encoding_of_str enc = match (String.lowercase enc) with
| "" -> None
| "utf-8" | "utf8" | "utf_8" -> Some Xmlm.UTF_8
| "utf-16" | "utf16" | "utf_16" -> Some Xmlm.UTF_16
| "utf-16be" | "utf16be" | "utf16_be" -> Some Xmlm.UTF_16BE
| "utf-16le" | "utf16le" | "utf16_le" -> Some Xmlm.UTF_16LE
| "iso-8859-1" | "iso88591" 
| "iso_8859_1" | "latin1" | "latin-1" -> Some Xmlm.ISO_8859_1 
| "ascii" | "us-ascii" -> Some Xmlm.US_ASCII
| e -> pr_err (str "unknown encoding '%s', trying to guess." e); None

let main () = 
  let usage = 
    str "Usage: %s <options> <files>\n\
         Reads xml files and outputs them on stdout.\n\
         Options:" exec 
  in
  let tree = ref false in
  let encoding = ref "" in
  let strip = ref false in
  let ename = ref false in
  let parse_only = ref false in
  let outline = ref false in
  let indent = ref false in
  let suffix = ref "" in
  let files = ref [] in
  let add_file s = files := s :: !files in
  let options = [
    "-t", Arg.Set tree, 
    "build document tree in memory.";
    "-enc", Arg.Set_string encoding, 
    "<enc>, use specified encoding, utf-8, utf-16, utf-16be, utf-16le,\n\
    \   iso-8859-1, ascii (otherwise guesses).";
    "-strip", Arg.Set strip, 
    "strip and collapse white space in character data.";
    "-ename", Arg.Set ename,
    "replace unknown entity references by their name (otherwise fails).";
    "-p", Arg.Set parse_only, 
    "parse only, no output.";
    "-ot", Arg.Set outline, 
    "output document ascii outline instead of xml.";
    "-indent", Arg.Set indent,
    "indent xml output.";
    "-trip", Arg.Set_string suffix,
    "<suffix>, result for file <file> is output to a file <file.suffix>."; ]
  in
  Arg.parse options add_file usage;
  let files = match (List.rev !files) with [] -> ["" (* stdin *) ] | l -> l in
  let enc = encoding_of_str !encoding in
  let indent = if !indent then Some 2 else None in
  process !tree enc !strip !ename !parse_only !outline indent !suffix files

let () = main ()

(*----------------------------------------------------------------------------
  Copyright (c) %%COPYRIGHTYEAR%%, Daniel C. Bünzli
  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are
  met:
        
  1. Redistributions of source code must retain the above copyright
     notice, this list of conditions and the following disclaimer.

  2. Redistributions in binary form must reproduce the above copyright
     notice, this list of conditions and the following disclaimer in the
     documentation and/or other materials provided with the
     distribution.

  3. Neither the name of the Daniel C. Bünzli nor the names of
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
  ----------------------------------------------------------------------------*)

