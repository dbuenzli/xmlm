(*----------------------------------------------------------------------------
   Copyright (c) %%COPYRIGHTYEAR%%, Daniel C. Bünzli. All rights reserved.
   Distributed under a BSD license, see license at the end of the file.
  ----------------------------------------------------------------------------*)

(*  Persistent XML cursor API whose support was dropped in 1.0.0 *)

type 'a tree = [ `El of 'a * Xmlm.tag * 'a tree list | `D of string ]

let input_tree ?enc ?strip ?names
    ?(entity = fun _ -> None)
    ?(prolog = fun _ -> ())
    ?(prune = fun _ -> false) 
    ?(d = fun s -> Some s) ?el default_label i =
  let prune tag _ = prune tag in
  let d data = function
    | (childs :: path') as path -> 
	begin match d data with 
	| Some data' -> ((`D data') :: childs) :: path'
	| None -> path 
	end
    | [] -> assert false
  in
  let s _ path = [] :: path in 
  let el' = match el with 
  | Some el -> el
  | None -> fun tag childs -> Some (`El (default_label, tag, childs))
  in
  let e tag = function
    | childs :: path -> 
	begin match el' tag (List.rev childs) with
	| Some el -> 
	    begin match path with
	    | parent :: path' -> (el :: parent) :: path'
	    | [] -> [ [ el ] ]
	    end
	| None -> path
	end	
    | [] -> assert false
  in
  match Xmlm.input ?enc ?strip ~entity ~prolog ~prune ~s ~d ~e [] i with
  | `Value [ [ root ] ] -> `Value (Some root)
  | `Value [ [] ] -> `Value None
  | `Error _ as e -> e
  | _ -> assert false

let output_tree ?(t = fun x -> Some x) o tree = 
  let rec aux o = function
    | (n :: next) :: path -> 
	begin match t n with
	| None -> aux o (next :: path)
	| Some n -> match n with
	  | `El (_, tag, childs) ->
	      Xmlm.output_signal o (`S tag);
	      aux o (childs :: next :: path)
	  | (`D d) as signal -> 
	      Xmlm.output_signal o signal;
	      aux o (next :: path)
	end
    | [] :: [] -> ()
    | [] :: path -> Xmlm.output_signal o `E; aux o path
    | [] -> assert false
  in
  aux o [ [ tree ] ]

(* Persistent cursor *)

type 'a loc = 
  | Root 
  | Path of ('a loc * 'a * Xmlm.tag) * 'a tree list * 'a tree list

type 'a cursor = 'a loc * 'a tree             (* Location and focused tree. *)

let err_root = "root element"
let err_data = "character data"
let cursor t = (Root, t)
let tree (l, t) = t

let rec root ((l, t) as c) = match l with
| Root -> c
| Path ((up, label, tag), prev, next) -> 
    root (up, `El (label, tag, List.rev_append prev (t :: next)))
	
let next (l, t) = match l with
| Path (up, prev, n :: next) -> Some (Path (up, t :: prev, next), n)
| _ -> None

let prev (l, t) = match l with
| Path (up, p :: prev, next) -> Some (Path (up, prev, t :: next), p)
| _ -> None

let up (l, t) = match l with
| Path ((up, label, tag), prev, next) ->
    Some (up, `El (label, tag, List.rev_append prev (t :: next)))
| Root -> None

let down (l, t) = match t with
| `El (label, tag, n :: next) -> Some (Path ((l, label, tag), [], next), n)
| _ -> None

let dnext c =
  let rec up_next c = match up c with
  | None -> raise Exit (* Root *)
  | Some c -> 
      match next c with
      | None -> up_next c 
      | (Some _) as some -> some
  in
  try match down c with
  | (Some _) as some -> some
  | None -> 
      match next c with
      | (Some _) as some -> some
      | None -> up_next c
  with Exit -> None
     
 let dprev c =
  let rec down_last c = match down c with
  | None -> c
  | Some c -> 
      let rec last c = match next c with
      | None -> c
      | Some c -> last c 
      in
      down_last (last c)
  in
  match prev c with
  | Some c -> Some (down_last c)
  | None -> 
      match up c with
      | None -> None (* Root *)
      | (Some _) as some -> some

let rec find mv p = function 
| None -> None 
| Some c ->  if p c then Some c else find mv p (mv c)
  
let cat l = String.concat "" l
let set_tree (l, t) t' = match t' with
| `El _ -> (l, t')
| `D d1 -> match l with                             (* Merge character data. *)
  | Path (up, `D d0 :: prev, `D d2 :: next) ->
      Path (up, prev, next), `D (cat [d0; d1; d2])
  | Path (up, `D d0 :: prev, next) -> 
      Path (up, prev, next), `D (cat [d0; d1])
  | Path (up, prev, `D d2 :: next) ->
      Path (up, prev, next), `D (cat [d1; d2])
  | _ -> (l, t')
	
let delete (l, t) = match l with
| Path (up, `D d0 :: prev, `D d1 :: next) -> 
    Path (up, prev, next), `D (cat [ d0; d1])       (* Merge character data. *)
| Path (up, p :: prev, next) -> Path (up, prev, next), p
| Path ((up, label, tag), [], next) -> (up, `El (label, tag, next))
| Root -> invalid_arg err_root

let insert_below (l, t) t' = match t with 
| `D _ -> invalid_arg err_data
| `El (label, tag, childs) -> 
    match t' with
    | `El _ -> Path ((l, label, tag), [], childs), t'
    | `D d0 -> match childs with                    (* merge Character data. *)
      | `D d1 :: next -> Path ((l, label, tag), [], next), `D (cat [d0; d1]) 
      | _ -> Path ((l, label, tag), [], childs), t' 
 
let insert_before (l, t) t' = match l with
| Root -> invalid_arg err_root
| Path (up, prev, next) -> match t' with
  | `El _ -> Path (up, prev, t :: next), t'
  | `D d1 ->                                        (* Merge character data. *)
      match prev with
      | `D d0 :: prev' ->
	  begin match t with
	  | `El _ -> Path (up, prev', t :: next), `D (cat [d0; d1])
	  | `D d2 -> Path (up, prev', next), `D (cat [d0; d1; d2])
	  end
      | _ -> 
	  match t with
	  | `El _ -> Path (up, prev, t :: next), t'
	  | `D d2 -> Path (up, prev, next), `D (cat [d1; d2]) 

let insert_after (l, t) t' = match l with
| Root -> invalid_arg err_root
| Path (up, prev, next) -> match t' with
  | `El _ -> Path (up, t :: prev, next), t'
  | `D d1 ->                                        (* Merge character data. *)
      match next with
      | `D d2 :: next' -> 
	  begin match t with
	  | `El _ -> Path (up, t :: prev, next'), `D (cat [d1; d2])
	  | `D d0 -> Path (up, prev, next'), `D (cat [d0; d1; d2])
	  end
      | _ -> 
	  match t with
	  | `El _ -> Path (up, t :: prev, next), t'
	  | `D d0 -> Path (up, prev, next), `D (cat [d0; d1])


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
