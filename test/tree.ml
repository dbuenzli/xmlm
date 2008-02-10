
(* This code is in public domain. *)

type t = [ `El of Xmlm.tag * t list | `D of string ]

let input ?enc ?strip ?entity ?prolog ?prune i = 
  let d data = function 
    | childs :: path -> ((`D data) :: childs) :: path 
    | [] -> assert false
  in
  let s _ path = [] :: path in
  let e tag = function
    | childs :: path -> 
        let el = `El (tag, List.rev childs) in
        begin match path with
        | parent :: path' -> (el :: parent) :: path' 
        | [] -> [ [ el ] ]
        end
    | [] -> assert false
  in
  match Xmlm.input ?enc ?strip ?entity ?prolog ~d ~s ~e [] i with
  | `Success [ [ root ] ] -> `Success (Some root)
  | `Success [ [] ]  -> `Success None (* the root was pruned *)
  | `Error _ as e -> e
  | _ -> assert false

let output o t = 
  let rec aux o = function
    | (n :: next) :: path -> 
	begin match n with
	| `El (tag, childs) -> 
            Xmlm.output_signal o (`S tag); 
            aux o (childs :: next :: path)
	| `D _ as d -> 
            Xmlm.output_signal o d;
            aux o (next :: path)
	end
    | [] :: [] -> ()
    | [] :: path -> Xmlm.output_signal o `E; aux o path
    | [] -> assert false
  in
  aux o [ [ t ] ]
