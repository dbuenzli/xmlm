(*----------------------------------------------------------------------------
   Copyright (c) %%COPYRIGHTYEAR%%, Daniel C. Bünzli. All rights reserved.
   Distributed under a BSD license, see license at the end of the file.
   Xmlm version %%VERSION%%
  ----------------------------------------------------------------------------*)

let str = Printf.sprintf

type error = [
  | `Max_buffer_size
  | `Unexpected_eoi
  | `Malformed_char_stream
  | `Unknown_encoding of string 
  | `Unknown_entity_ref of string
  | `Unknown_ns_prefix of string
  | `Illegal_char_ref of string
  | `Illegal_char_seq of string
  | `Expected_char_seqs of string list * string
  | `Expected_root_element ]

let error_message = function
  | `Max_buffer_size -> "maximal buffer size exceeded"
  | `Unexpected_eoi -> "unexpected end of input"
  | `Malformed_char_stream -> "malformed character stream"
  | `Unknown_encoding e -> str "unknown encoding (%s)" e
  | `Unknown_entity_ref e -> str "unknown entity reference (%s)" e
  | `Unknown_ns_prefix e -> str "unknown namespace prefix (%s)" e
  | `Illegal_char_ref s -> str "illegal character reference (#%s)" s
  | `Illegal_char_seq s -> str "character sequence \"%s\" illegal here" s
  | `Expected_root_element -> "expected root element"
  | `Expected_char_seqs (exps, fnd) -> 
      let exps = List.fold_left (fun s v -> str "%s\"%s\", " s v) "" exps in
      str "expected character sequence %sfound \"%s\"" exps fnd

exception Error of (int * int) * error                     (* internal only *)
exception Malformed                      (* for char streams, internal only *)

type encoding = [ `UTF_8 | `UTF_16 | `UTF_16BE | `UTF_16LE | `ISO_8859_1 | 
                  `US_ASCII ]

type dtd = string option
type name = string * string
type attribute = name * string
type tag = name * attribute list 
type limit =                            (* XML is a strange beast to parse. *) 
  | Stag of name   (* '<' qname *) 
  | Etag of name   (* '</' qname *) 
  | Pi of name     (* '<?' qname *) 
  | Comment        (* '<!--' *)
  | Cdata          (* '<![CDATA[' *)
  | Dtd            (* '<!' *) 
  | Text           (* other character *)
  | Eoi            (* End of input *)

let name_str (p,l) = if p <> "" then str "%s:%s" p l else str "%s" l

let ns_xml = "http://www.w3.org/XML/1998/namespace"
let ns_xmlns = "http://www.w3.org/2000/xmlns/"

(* Input *)

type input = unit -> int

let input_of_fun f = f
let input_of_channel ic = fun () -> input_byte ic 
let input_of_string ?(pos = 0) s = 
  let len = String.length s in 
  let pos = ref (pos - 1) in 
  fun () -> 
    incr pos; 
    if !pos = len then raise End_of_file else Char.code s.[!pos]

type 'a parse = 
    { i : input;
      strip : bool;                        (* Strip and collapse whitespace. *)
      fun_entity : string -> string option;               (* User callbacks. *)
      fun_prolog : dtd -> unit;                          
      fun_prune : tag -> 'a -> bool;
      fun_start : tag -> 'a -> 'a;
      fun_data : string -> 'a -> 'a;
      fun_end : tag -> 'a -> 'a;
      mutable accum : 'a; 
      mutable uchar : input -> int;                           (* Char lexer. *)
      mutable line : int;
      mutable col : int;
      mutable eoi : bool;                         (* True when end of input. *)
      mutable u : int;                               (* Character lookahead. *)
      mutable cr : bool;                         (* True if last u was '\r'. *)
      ident : Buffer.t;                        (* For names and entity refs. *)
      attr : Buffer.t;                                (* For attribute data. *)
      data : Buffer.t;                                (* For character data. *)
      mutable dlast_white : bool;  (* True if last character data was white. *)
      mutable limit : limit;                           (* Last parsed limit. *)
      mutable indata : bool;            (* True when parsing character data. *)
      mutable prune : bool;          (* True when callbacks are not invoked. *)
      mutable path : (tag * bool) list }           (* Stack of (tag, prune). *)

(* Character encodings *)

let utf8_len = [|        (* Char byte length according to first UTF-8 byte. *)
  1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1;
  1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1;
  1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1;
  1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1;
  1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 0; 0;
  0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
  0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
  0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2;
  2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3;
  3; 3; 3; 3; 3; 3; 4; 4; 4; 4; 4; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 |]

let add_uchar b u =  
  (* UTF-8 encodes an uchar in the buffer, assumes u is valid code point. *)
  let buf c = Buffer.add_char b (Char.chr c) in
  if u <= 0x007F then 
    buf u 
  else if u <= 0x07FF then 
    (buf (0xC0 lor (u lsr 6)); 
     buf (0x80 lor (u land 0x3F)))
  else if u <= 0xFFFF then
    (buf (0xE0 lor (u lsr 12));
     buf (0x80 lor ((u lsr 6) land 0x3F));
     buf (0x80 lor (u land 0x3F)))
  else
    (buf (0xF0 lor (u lsr 18));
     buf (0x80 lor ((u lsr 12) land 0x3F));
     buf (0x80 lor ((u lsr 6) land 0x3F));
     buf (0x80 lor (u land 0x3F)))

let uchar_utf8 i =
  let b0 = i () in
  begin match utf8_len.(b0) with
  | 0 -> raise Malformed
  | 1 -> b0
  | 2 ->
      let b1 = i () in
      if b1 lsr 6 != 0b10 then raise Malformed else
      ((b0 land 0x1F) lsl 6) lor (b1 land 0x3F)
  | 3 ->
      let b1 = i () in
      let b2 = i () in
      if b2 lsr 6 != 0b10 then raise Malformed else
      begin match b0 with
      | 0xE0 -> if b1 < 0xA0 || 0xBF < b1 then raise Malformed else ()
      | 0xED -> if b1 < 0x80 || 0x9F < b1 then raise Malformed else ()
      | _ -> if b1 lsr 6 != 0b10 then raise Malformed else ()
      end;
      ((b0 land 0x0F) lsl 12) lor ((b1 land 0x3F) lsl 6) lor (b2 land 0x3F)
  | 4 -> 
      let b1 = i () in
      let b2 = i () in
      let b3 = i () in
      if  b3 lsr 6 != 0b10 || b2 lsr 6 != 0b10 then raise Malformed else
      begin match b0 with
      | 0xF0 -> if b1 < 0x90 || 0xBF < b1 then raise Malformed else ()
      | 0xF4 -> if b1 < 0x80 || 0x8F < b1 then raise Malformed else ()
      | _ -> if b1 lsr 6 != 0b10 then raise Malformed else ()
      end;
      ((b0 land 0x07) lsl 18) lor ((b1 land 0x3F) lsl 12) lor 
      ((b2 land 0x3F) lsl 6) lor (b3 land 0x3F)
  | _ -> assert false	
  end

let int16_be i = 
  let b0 = i () in
  let b1 = i () in
  (b0 lsl 8) lor b1
    
let int16_le i = 
  let b0 = i () in
  let b1 = i () in
  (b1 lsl 8) lor b0 

let uchar_utf16 int16 i = 
  let c0 = int16 i in
  if c0 < 0xD800 || c0 > 0xDFFF then c0 else
  if c0 >= 0xDBFF then raise Malformed else
  let c1 = int16 i in
  (((c0 land 0x3FF) lsl 10) lor (c1 land 0x3FF)) + 0x10000

let uchar_byte i = i ()
let uchar_iso_8859_1 i = i ()
let uchar_ascii i = 
  let b = i () in if b > 127 then raise Malformed else b

let stream_utf8 p = p.uchar <- uchar_utf8 
let stream_utf16be p = p.uchar <- uchar_utf16 int16_be
let stream_utf16le p = p.uchar <- uchar_utf16 int16_le 
let stream_iso_8859_1 p = p.uchar <- uchar_iso_8859_1
let stream_ascii p = p.uchar <- uchar_ascii

let u_nl = 0x000A     (* newline *)
let u_cr = 0x000D     (* carriage return *)
let u_space = 0x0020  (* carriage return *)
let u_quot = 0x0022   (* quote *)
let u_sharp = 0x0023  (* # *)
let u_amp = 0x0026    (* & *)
let u_apos = 0x0027   (* ' *)
let u_minus = 0x002D  (* - *)
let u_slash = 0x002F  (* / *)
let u_colon = 0x003A  (* : *)
let u_scolon = 0x003B (* ; *)
let u_lt = 0x003C     (* < *)
let u_eq = 0x003D     (* = *)
let u_gt = 0x003E     (* > *)
let u_qmark = 0x003F  (* ? *)
let u_emark = 0x0021  (* ! *)
let u_lbrack = 0x005B (* [ *)
let u_rbrack = 0x005D (* ] *)
let u_x = 0x0078      (* x *)

(* Bracketed non-terminals in comments refer to XML 1.0 non terminals *)

let r : int -> int -> int -> bool = fun u a b -> a <= u && u <= b
let is_white = function 0x0020 | 0x0009 | 0x000D | 0x000A -> true | _ -> false
let is_char = function                                            (* {Char} *)
  | 0x0009 | 0x000A | 0x000D -> true
  | u when r u 0x0020 0xD7FF || r u 0xE000 0xFFFD 
  || r u 0x10000 0x10FFFF -> true
  | _ -> false

let comm_range u = r u 0x00C0 0x00D6           (* common to functions below *)
|| r u 0x00D8 0x00F6 || r u 0x00F8 0x02FF || r u 0x0370 0x037D 
|| r u 0x037F 0x1FFF || r u 0x200C 0x200D || r u 0x2070 0x218F
|| r u 0x2C00 0x2FEF || r u 0x3001 0xD7FF || r u 0xF900 0xFDCF 
|| r u 0xFDF0 0xFFFD || r u 0x10000 0xEFFFF

let is_name_start_char = function        (* {NameStartChar} - ':' (XML 1.1) *)
  | 0x005F -> true                                                   (* '_' *) 
  | u when r u 0x0061 0x007A || r u 0x0041 0x005A -> true  (* [a-z] | [A-Z] *)
  | u when is_white u -> false
  | u when comm_range u -> true 
  | _ -> false

let is_name_char = function                   (* {NameChar} - ':' (XML 1.1) *)
  | 0x005F | 0x002D | 0x002E | 0x00B7 -> true                (* '_' '-' '.' *) 
  | u when r u 0x0061 0x007A  || r u 0x0041 0x005A         (* [a-z] | [A-Z] *) 
  || r u 0x0030 0x0039 -> true                                     (* [0-9] *)
  | u when is_white u -> false
  | u when comm_range u || r u 0x0300 0x036F || r u 0x203F 0x2040 -> true
  | _ -> false

let utf8_str u = let b = Buffer.create 4 in add_uchar b u; Buffer.contents b
let err p e = raise (Error ((p.line, p.col), e))
let err_illegal_char p u = err p (`Illegal_char_seq (utf8_str u))
let err_expected_seqs p exps s = err p (`Expected_char_seqs (exps, s))
let err_expected_chars p exps = 
  err p (`Expected_char_seqs (List.map utf8_str exps, utf8_str p.u))
  
let rec nextc p =                    
  if p.eoi then err p `Unexpected_eoi;
  if p.u = u_nl then (p.line <- p.line + 1; p.col <- 1) else p.col <- p.col + 1;
  try 
    p.u <- p.uchar p.i;
    if not (is_char p.u) then raise Malformed;
    if p.cr && p.u = u_nl then p.u <- p.uchar p.i;         (* cr nl business *)
    if p.u = u_cr then (p.cr <- true; p.u <- u_nl) else p.cr <- false
  with
  | End_of_file -> p.eoi <- true; p.u <- 0
  | Malformed -> err p `Malformed_char_stream

let skip_white p = while (is_white p.u) do nextc p done        
let accept p u = if p.u = u then nextc p else err_expected_chars p [ u ]

(* Buffer functions with white space stripping and collapsing *)

let baddc = Buffer.add_char
let baddc_strip b last_white c = 
  if is_white (Char.code c) then (if not last_white then baddc b ' '; true) else
  (baddc b c; false)

let badds_strip b last_white s = 
  let last_white = ref last_white in
  for i = 0 to (String.length s) - 1 do 
    last_white := baddc_strip b !last_white s.[i] 
  done;
  !last_white 

let baddu_strip b last_white u = 
  if is_white u then (if not last_white then baddc b ' '; true) else
  (add_uchar b u; false)

let bcont_strip b last_white = 
  if not last_white then Buffer.contents b else
  let len = Buffer.length b in
  if len = 0 then "" else Buffer.sub b 0 (len - 1)
  
(* Character data buffer functions *)

let data_adds p s = 
  if p.strip then p.dlast_white <- badds_strip p.data p.dlast_white s else
  Buffer.add_string p.data s 

let data_addu p u = 
  if p.strip then p.dlast_white <- baddu_strip p.data p.dlast_white u else
  add_uchar p.data u

let data_cont p = 
  if p.strip then bcont_strip p.data p.dlast_white else
  Buffer.contents p.data

let data_clear p = Buffer.clear p.data; p.dlast_white <- true
								 
(* Parse callbacks invocations *)

let data_start p = 
  if not p.prune && not p.indata then (p.indata <- true; data_clear p)

let data_end p = 
  if p.indata then begin
    let d = data_cont p in 
    if d <> "" then p.accum <- p.fun_data d p.accum 
  end;
  p.indata <- false

let tag_start p tag = 
  p.path <- (tag, p.prune) :: p.path;
  if not p.prune then begin
    if p.fun_prune tag p.accum then p.prune <- true else
    (data_end p; p.accum <- p.fun_start tag p.accum)
  end

let tag_end p name = match p.path with
| t :: path' -> 
    begin match t with
    | ((n, _) as tag), prune when (n = name) ->
	if not p.prune then (data_end p; p.accum <- p.fun_end tag p.accum);
	p.prune <- prune; p.path <- path'
    | (n, _), _ ->                                          (* Tag mismatch. *)
	err_expected_seqs p [ (name_str n) ] (name_str name) 
    end
| [] -> assert false

let p_ncname p =                                 (* {NCName} (Namespace 1.1) *)	
  Buffer.clear p.ident;
  if not (is_name_start_char p.u) then err_illegal_char p p.u;
  add_uchar p.ident p.u; nextc p;
  while is_name_char p.u do add_uchar p.ident p.u; nextc p done;
  Buffer.contents p.ident
    
let p_qname p =                                   (* {QName} (Namespace 1.1) *)
  let n = p_ncname p in
  if p.u <> u_colon then ("", n) else (accept p u_colon; (n, p_ncname p))

let p_charref p =                               (* {CharRef}, '&' was eaten. *) 
  Buffer.clear p.ident;
  accept p u_sharp;
  while (p.u <> u_scolon) do add_uchar p.ident p.u; nextc p done;
  accept p u_scolon;
  let c = Buffer.contents p.ident in
  try 
    if c = "" then raise (Failure "");
    let u = int_of_string (if c.[0] = 'x' then ("0" ^ c) else c) in
    if not (is_char u) then raise (Failure "");
    Buffer.clear p.ident; 
    add_uchar p.ident u;                                    (* UTF-8 encode. *)
    Buffer.contents p.ident
  with Failure _ -> err p (`Illegal_char_ref c) 

let predefined_entities = 
  let h = Hashtbl.create 5 in
  let e = Hashtbl.add h  in
  e "lt" "<"; e "gt" ">"; e "amp" "&"; e "apos" "'"; e "quot" "\""; h

let p_entity_ref p =                          (* {EntityRef}, '&' was eaten. *)
  let ent = p_ncname p in
  if p.u <> u_scolon then err_expected_chars p [ u_scolon ] else
  try 
    accept p u_scolon; 
    Hashtbl.find predefined_entities ent
  with 
  | Not_found -> 
      match p.fun_entity ent with
      | Some s -> s
      | None -> err p (`Unknown_entity_ref ent)

let p_reference p =                                           (* {Reference} *)
  accept p u_amp; if p.u = u_sharp then p_charref p else p_entity_ref p

let p_attr_value p =                                      (* {S}? {AttValue} *)
  Buffer.clear p.attr;
  skip_white p;
  let delim = match p.u with 
  | (0x0022 | 0x0027) (* quote or apos *) -> p.u 
  | _ -> err_expected_chars p [ u_quot; u_apos]
  in
  accept p delim;
  if p.prune then (while (p.u <> delim) do nextc p done; accept p delim; "") 
  else begin
    let last_white = ref true (* space may be coming from a charref *) in
    skip_white p;
    while (p.u <> delim) do
      match p.u with
      | 0x0026 (* & *) -> 
	  last_white := badds_strip p.attr !last_white (p_reference p)
      | 0x003C (* < *) -> 
	  err_illegal_char p u_lt
      | _ -> 
	  last_white := baddu_strip p.attr !last_white p.u; nextc p
    done;
    accept p delim;
    bcont_strip p.attr !last_white
  end
  
let p_attributes p =             (* ({S} {Attribute})* {S}? *) 
  let rec aux p accum = 
    if not (is_white p.u) then List.rev accum else
    begin
      skip_white p;
      match p.u with
      | 0x002F (* / *) | 0x003E (* > *) -> List.rev accum
      | _ -> 
	  let n = p_qname p in
	  skip_white p;
	  accept p u_eq;
	  aux p ((n, (p_attr_value p)) :: accum)
    end
  in
  aux p []

let rec skip_comment p =                      (* {Comment}, '<!--' was eaten *)
  while (p.u <> u_minus) do nextc p done; 
  accept p u_minus;
  if p.u <> u_minus then skip_comment p else (accept p u_minus; accept p u_gt)
    
let rec skip_pi p =                            (* {PI}, '<?' qname was eaten *)
  while (p.u <> u_qmark) do nextc p done;
  accept p u_qmark;
  if p.u <> u_gt then skip_pi p else accept p u_gt

let p_chardata p = (* {CharData}* ({Reference}{Chardata})* *)
  if p.prune then while (p.u <> u_lt) do nextc p done else
  while (p.u <> u_lt) do 
    match p.u with
    | 0x0026 (* & *) -> data_adds p (p_reference p)
    | 0x005D (* ] *) -> 
	data_addu p p.u;
	accept p u_rbrack;
	if p.u = u_rbrack then begin 
	  data_addu p p.u;
	  accept p u_rbrack;                       (* detects ']'*']]>' *)
	  while (p.u = u_rbrack) do data_addu p p.u; accept p u_rbrack done;
	  if p.u = u_gt then err p (`Illegal_char_seq "]]>");
	end
    | _ -> data_addu p p.u; nextc p
  done
    
let rec p_cdata p =                                       (* {CData} {CDEnd} *)
  if p.prune then 
    try while (true) do 
      if p.u = u_rbrack then begin
	accept p u_rbrack;
	if p.u = u_rbrack then begin 
	  accept p u_rbrack;
	  if p.u = u_gt then (accept p u_gt; raise Exit)
	end
      end;
      nextc p;
    done with Exit -> ()
  else
    try while (true) do 
      if p.u = u_rbrack then begin
	accept p u_rbrack;
	if p.u = u_rbrack then begin 
	  accept p u_rbrack;
	  if p.u = u_gt then (accept p u_gt; raise Exit);
	  data_addu p (Char.code ']')
	end;
	data_addu p (Char.code ']');
      end;
      data_addu p p.u;
      nextc p;
    done with Exit -> ()

let p_limit p =                                    (* Parses a markup limit *)
  p.limit <-
    if p.eoi then Eoi else
    if p.u <> u_lt then Text else 
    match (accept p u_lt; p.u) with
    | 0x002F (* / *) -> accept p u_slash; Etag (p_qname p)
    | 0x003F (* ? *) -> accept p u_qmark; Pi (p_qname p)
    | 0x0021 (* ! *) -> accept p u_emark;
	begin match p.u with
	| 0x002D (* - *) -> accept p u_minus; accept p u_minus; Comment
	| 0x005B (* [ *) -> 
	    accept p u_lbrack;
	    Buffer.clear p.ident;
	    for i = 1 to 6 do add_uchar p.ident p.u; nextc p done;
	    let cdata = Buffer.contents p.ident in
	    if cdata = "CDATA[" then Cdata else
	    err_expected_seqs p ["CDATA["] cdata
	| 0x0044 (* D *) -> Dtd
	| c -> err p (`Illegal_char_seq (str "<!%s" (utf8_str c)))
	end
    | _ -> Stag (p_qname p)

let rec p_element p =    (* {element},  '<'qname was eaten *) 
  begin match p.limit with
  | Stag n ->       
      tag_start p (n, p_attributes p);
      skip_white p;
      begin match p.u with 
      | 0x002F (* / *) ->                                  (* {EmptyElemTag} *)
	  tag_end p n;
	  accept p u_slash; 
	  if p.u <> u_gt then err_expected_chars p [ u_gt ];
	  if p.path <> [] then accept p u_gt     (* Not end of root element. *) 
      | 0x003E (* > *) -> accept p u_gt                            (* {Stag} *)
      | _ -> err_expected_chars p [u_slash; u_gt]
      end
  | Etag n -> 
      tag_end p n; 
      skip_white p; 
      if p.u <> u_gt then err_expected_chars p [ u_gt ];
      if p.path <> [] then accept p u_gt         (* Not end of root element. *) 
  | Pi _ -> skip_pi p
  | Comment -> skip_comment p
  | Text -> data_start p; p_chardata p
  | Cdata -> data_start p; p_cdata p
  | Dtd -> err p (`Illegal_char_seq "<!D")
  | Eoi -> err p `Unexpected_eoi
  end;
  if p.path <> [] then (p_limit p; p_element p)

let rec skip_misc p = match p.limit with
| Pi _ -> skip_pi p; skip_white p; p_limit p; skip_misc p
| Comment -> skip_comment p; skip_white p; p_limit p; skip_misc p
| _ -> ()

let p_dtd p =                               (* {Misc}* {doctypedecl} {Misc}* *)
  skip_misc p;
  if p.limit <> Dtd then p.fun_prolog None else
  let nest = ref 1 in                               
  Buffer.clear p.data;
  Buffer.add_string p.data "<!";
  while (!nest > 0) do match p.u with                 (* Rough DTD parsing. *)
  | 0x003C (* < *) -> 
      accept p u_lt;
      if p.u <> u_emark then (Buffer.add_char p.data '<'; incr nest) else
      begin                                     
	accept p u_emark;                       
	if p.u <> u_minus then (Buffer.add_string p.data "<!"; incr nest) else 
	begin 
	  accept p u_minus;                     (* Carefull with comments ! *) 
	  if p.u <> u_minus then (Buffer.add_string p.data "<!-"; incr nest)else
	  (accept p u_minus; skip_comment p)
	end
      end
  | 0x003E (* > *) -> Buffer.add_char p.data '>'; accept p u_gt; decr nest
  | (0x0022 | 0x0027) as d ->                                (* quote, apos *)
      add_uchar p.data p.u; accept p d;
      while p.u <> d do add_uchar p.data p.u; nextc p done; 
      add_uchar p.data p.u; accept p d;
    | _ -> add_uchar p.data p.u; nextc p
  done;
  p.fun_prolog (Some (Buffer.contents p.data));
  skip_white p;
  p_limit p;
  skip_misc p
    
let p_xml_decl p ~ignore_enc =                                (* {XMLDecl}? *)
  let yes_no = ["yes"; "no"] in
  let p_val p = skip_white p; accept p u_eq; skip_white p; p_attr_value p in
  let p_val_exp p exp = 
    let v = p_val p in if not (List.mem v exp) then err_expected_seqs p exp v
  in
  p_limit p;
  if p.limit = Pi ("", "xml") then begin
    let v = skip_white p; p_ncname p in
    if v <> "version" then err_expected_seqs p [ "version" ] v;
    p_val_exp p ["1.0"; "1.1"];
    skip_white p;
    if p.u <> u_qmark then begin 
      match p_ncname p with
      | "encoding" -> 
	  let enc = p_val p in
	  if not ignore_enc then begin 
	    match (String.lowercase enc) with
	    | "utf-8" -> stream_utf8 p
	    | "utf-16be" -> stream_utf16be p
	    | "utf-16le" -> stream_utf16le p
	    | "iso-8859-1" -> stream_iso_8859_1 p
	    | "us-ascii" | "ascii" -> stream_ascii p
	    | "utf-16" -> err p `Malformed_char_stream  (* We need a BOM. *)
	    | enc -> err p (`Unknown_encoding enc)
	  end;
	  skip_white p;
	  if p.u <> u_qmark then begin 
	    match p_ncname p with
	    | "standalone" ->  p_val_exp p yes_no; 
	    | n -> err_expected_seqs p ["standalone"; "?>" ] n 
	  end
      | "standalone" -> p_val_exp p yes_no
      | n -> err_expected_seqs p [ "encoding"; "standalone"; "?>" ] n
    end;
    skip_white p;
    accept p u_qmark;
    accept p u_gt;
    p_limit p
  end;
  if p.limit = Text && is_white p.u then (skip_white p; p_limit p)

let find_encoding p enc =                                  (* Encoding mess. *)
  let reset stream p = stream p; p.col <- 0; nextc p in 
  match enc with
  | None ->                                   (* User doesn't know encoding. *)
      begin match nextc p; p.u with          
      | 0xFE ->                                             (* UTF-16BE BOM. *)
	  nextc p; if p.u <> 0xFF then err p `Malformed_char_stream;
	  reset stream_utf16be p;
	  true                                 
      | 0xFF ->                                             (* UTF-16LE BOM. *)
	  nextc p; if p.u <> 0xFE then err p `Malformed_char_stream;
	  reset stream_utf16le p;
	  true                                 
      | 0x3C | _ ->                      (* UTF-8 or other, try declaration. *)
	  stream_utf8 p; 
	  false  
      end
  | Some e ->                                        (* User knows encoding. *)
      begin match e with                              
      | `US_ASCII -> reset stream_ascii p
      | `ISO_8859_1 -> reset stream_iso_8859_1 p
      | `UTF_8 -> reset stream_utf8 p
      | `UTF_16 ->                               (* Which UTF-16 ? look BOM. *)
	  let b0 = nextc p; p.u in
	  let b1 = nextc p; p.u in
	  begin match b0, b1 with                
	  | 0xFE, 0xFF -> reset stream_utf16be p
	  | 0xFF, 0xFE -> reset stream_utf16le p
	  | _ -> err p `Malformed_char_stream;
	  end
      | `UTF_16BE ->                                 (* Skip BOM if present. *)
	  reset stream_utf16be p; if p.u = 0xFEFF then (p.col <- 0; nextc p)
      | `UTF_16LE ->
	  reset stream_utf16le p; if p.u = 0xFEFF then (p.col <- 0; nextc p)
      end;
      true                                        (* Ignore xml declaration. *)
    
let input ?(enc = None) ?(strip = false) 
    ?(ns = fun _ -> None)
    ?(entity = fun _ -> None)
    ?(prolog = fun _ -> ())
    ?(prune = fun _ _ -> false) 
    ?(s = fun _ acc -> acc) 
    ?(e = fun _ acc -> acc) 
    ?(d = fun _ acc -> acc) accum i = 
  let p = 
    { i = i;
      strip = strip;
      fun_entity = entity;
      fun_prolog = prolog;
      fun_prune = prune;
      fun_start = s;
      fun_data = d;
      fun_end = e; 
      accum = accum; 
      uchar = uchar_byte;
      line = 1;
      col = 0;
      eoi = false;
      u = 0x0000;
      cr = false;
      ident = Buffer.create 64;
      attr = Buffer.create 128;
      data = Buffer.create 1024;
      dlast_white = true;
      prune = false;
      indata = false;
      path = [];
      limit = Text; }
  in
  try
    p_xml_decl p ~ignore_enc:(find_encoding p enc);
    p_dtd p;
    match p.limit with
    | Stag _ -> p_element p; `Success p.accum
    | _ -> err p `Expected_root_element
  with
  | Error (pos, e) -> `Error (pos, e)
  | Failure "Buffer.add: cannot grow buffer" ->    (* This is brittle. *)
      `Error ((p.line, p.col),  `Max_buffer_size)


let input_tree ?enc ?strip ?ns ?entity ?prolog 
    ?(prune = fun _ -> false) ~el ~d i = 
  let d data = function 
    | childs :: path -> ((d data) :: childs) :: path 
    | [] -> assert false
  in
  let s _ path = [] :: path in
  let e tag = function
    | childs :: path -> 
        let el = el tag (List.rev childs) in
        begin match path with
        | parent :: path' -> (el :: parent) :: path' 
        | [] -> [ [ el ] ]
        end
    | [] -> assert false
  in
  let prune tag _ = prune tag in
  match input ?enc ?strip ?ns ?entity ?prolog ~prune ~d ~s ~e [] i with
  | `Success [ [ root ] ] -> `Success (Some root)
  | `Success [ [] ]  -> `Success None (* the root was pruned *)
  | `Error _ as e -> e
  | _ -> assert false


(* Output *)

type signal = [ `S of tag | `D of string | `E ]
type 'a tree = [ `El of tag * 'a list | `D of string ]
type output = 
    { out : string -> int -> int -> unit;
      outc : char -> unit;
      mutable s_last : tag option;            (* Some if last signal was `S. *)
      mutable indent : int option;
      mutable depth : int;                                 (* Nesting depth. *)
      mutable nest : name list }                            (* Name nesting. *)

let output_of ?(indent = None) ?(prefix = fun _ -> None) out outc = 
  { out = out; outc = outc; s_last = None; indent = indent; depth = 0; 
    nest = [] }

let output_of_channel ?indent ?prefix oc = 
  output_of ?indent ?prefix (output oc) (output_char oc)

let output_of_buffer ?indent ?prefix b = 
  output_of ?indent ?prefix (Buffer.add_substring b) (Buffer.add_char b)

let output_of_fun ?indent ?prefix f =
  let out = fun s p l -> for i = p to p + l - 1 do f (Char.code s.[p]) done in
  let outc = fun c -> f (Char.code c) in
  output_of ?indent ?prefix out outc

let err_el_open n = str "unclosed element (%s) on output" (name_str n)
let err_el_end = str "end of element without matching start element"
let err_no_root = "character data written outside root element"

let outs o s = o.out s 0 (String.length s)
let out_name o (p,l) = if p <> "" then (outs o p; o.outc ':'); outs o l
let out_data o s =                             
  let len = String.length s in
  let start = ref 0 in
  let last = ref 0 in
  let escape e = 
    o.out s !start (!last - !start);
    outs o e;
    incr last;
    start := !last
  in
  while (!last < len) do match s.[!last] with   (* Escape markup delimiters. *)
  | '<' -> escape "&lt;"
  | '>' -> escape "&gt;"
  | '&' -> escape "&amp;"
(*| '\'' -> escape "&apos;" *)     (* Not needed we use \x22 for attributes. *)
  | '\x22' -> escape "&quot;"
  | _ -> incr last
  done;
  o.out s !start (!last - !start)

let out_attribute o (n,v)  =
  o.outc ' '; out_name o n; outs o "=\x22"; out_data o v; o.outc '\x22'
    
let out_start o (n, atts) = 
  o.outc '<'; out_name o n; List.iter (out_attribute o) atts; o.outc '>'
    
let out_end o n = outs o "</"; out_name o n; o.outc '>'    
let out_startend o (n, atts) = 
  o.outc '<'; out_name o n; List.iter (out_attribute o) atts; outs o "/>"

let output_prolog o dtd =
  outs o "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n";
  match dtd with Some dtd -> (outs o dtd; o.outc '\n') | None -> ()

let output_signal o sgn = 
  let out o d f v = match o.indent with
  | None -> f o v
  | Some c -> for i = 1 to (d * c) do o.outc ' ' done; f o v; o.outc '\n'
  in
  let out_s_last o = match o.s_last with 
  | None -> () | Some tag -> out o (o.depth - 1) out_start tag
  in
  match sgn with
  | `S ((n, _) as tag) -> 
      out_s_last o;
      o.depth <- o.depth + 1;
      o.nest <- n :: o.nest;
      o.s_last <- Some tag              (* Delayed to handle empty elements. *)
  | `E -> 
      begin match o.nest with
      | n :: nest' -> 
	  let depth' = o.depth - 1 in 
	  begin match o.s_last with
	  | None -> out o depth' out_end n
	  | Some tag -> out o depth' out_startend tag
	  end;
	  o.depth <- depth';
	  o.nest <- nest'; 
	  o.s_last <- None;
      | [] -> invalid_arg err_el_end
      end
  | `D d -> 
      if o.nest = [] then invalid_arg err_no_root else
      out_s_last o;
      out o o.depth out_data d;
      o.s_last <- None

let output_tree fold o t = 
  let rec aux o = function
    | (n :: next) :: path -> 
	begin match fold n with
	| `El (tag, childs) -> 
            output_signal o (`S tag); 
            aux o (childs :: next :: path)
	| `D _ as d -> 
            output_signal o d;
            aux o (next :: path)
	end
    | [] :: [] -> ()
    | [] :: path -> output_signal o `E; aux o path
    | [] -> assert false
  in
  aux o [ [ t ] ]

let output_finish ?(nl = false) o = match o.nest with 
| n :: _ -> invalid_arg (err_el_open n) 
| [] -> if nl then o.outc '\n'
    
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
