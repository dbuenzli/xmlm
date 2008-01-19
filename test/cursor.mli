(*----------------------------------------------------------------------------
   Copyright (c) %%COPYRIGHTYEAR%%, Daniel C. Bünzli. All rights reserved.
   Distributed under a BSD license, see license at the end of the file.
  ----------------------------------------------------------------------------*)

(** Persistent XML cursor (unsupported). 

    Persistent XML cursor API whose support was dropped in 1.0.0 

    The {{:#curs}cursor} allows to navigate and update a
    {{:#TYPEtree}simple} in-memory tree representation of the root
    element. Updates performed by the cursor are persistent (non
    destructive).

    {b References.}

    G. Huet. {e {{:http://dx.doi.org/10.1017/S0956796897002864}The Zipper}. 
    J. Functional Programming}, 7 (5), Sept 1997, pp. 549-554.
*)

type 'a tree = [ `El of 'a * Xmlm.tag * 'a tree list | `D of string ]
(** The type for trees. Either an element ([`El]) or character data ([`D]). 
    The type ['a] is for user labels. *) 

val input_tree : ?enc:Xmlm.encoding option -> ?strip:bool -> 
   ?entity: (string -> string option) ->
   ?prolog: (Xmlm.dtd -> unit) ->
   ?prune:(Xmlm.tag -> bool) -> 
   ?d:(string -> string option) -> 
   ?el:(Xmlm.tag -> 'a tree list -> 'a tree option) -> 'a -> Xmlm.input -> 
[ `Value of 'a tree option | `Error of (int * int) * Xmlm.error ]
 (** Inputs an XML document and returns a tree representation of 
     of the {{:http://www.w3.org/TR/REC-xml/#dt-root} root element} 
     or [None] if it was pruned. The value of type ['a]
     is the default label used whenever the argument [el] is left unspecified.
     {ul 
     {- [enc], character encoding of the document, {{:#input} details}. 
	Defaults to [None].}
     {- [strip], strips whitespace in character data, {{:#input} details}.
	Defaults to [false].} 
     {- [entity] is called to resolve non predefined entity references, 
        {{:#input} details}. Default returns [None].}
     {- [prolog] is called with the optional DTD just after the document
        {{:http://www.w3.org/TR/REC-xml/#NT-prolog}prolog} was parsed.
        Default does nothing.}
     {- [prune] is called whenever a new element starts. If it returns [true],
        no callbacks are invoked for the element and its children, 
       {{:#input} details}. Default returns [false].}
     {- [d], is called on character data, if [None] is returned, the given 
        character data is not included in the resulting tree. 
        Default returns the argument.}
     {- [el] is called to construct an element. The function
        is given the element tag and children, it
        must return an optional tree (this allows to prune an element after
        the data has been seen). Default constructs the element and labels
        it with the value of type ['a] given to {!input_tree}.}} *)


val output_tree : ?t:('a tree -> 'a tree option) -> Xmlm.output -> 'a tree -> 
  unit
(** Outputs signals corresponding to the given tree. [t] is called
    before an element or character data is written, the result
    is actually written ([None] outputs nothing), default returns the
    argument. *)

(** {1:curs Persistent cursor} *)

type 'a cursor
(** The type for cursors.

    A cursor points on a subtree of a larger tree, the {e complete
    tree}.  In what follows we {e identify the cursor with the subtree it
    points on}. The following concepts are defined with respect to a
    cursor.
    {ul 
    {- The {e parent} denotes the unique element in which the cursor
       is contained. } 
    {- An {e ancestor}, is either the cursor's parent or an ancestor's parent.}
    {- The {e root}, is the cursor's ancestor without parent.}
    {- A {e sibling} denotes a tree whose parent is the same as the cursor.}
    {- A {e children} denotes a tree whose parent is the cursor.}  
    {- A {e descendent} denotes a tree which has the cursor as an ancestor. }}

    Note that not all these concepts may exist for a given cursor. *)

val cursor : 'a tree -> 'a cursor
(** Returns a cursor on the given tree. *)

val tree : 'a cursor -> 'a tree
(** Returns the tree pointed by the cursor. *)

(** {2 Move and search} *)

val root : 'a cursor -> 'a cursor
(** Moves to the root. *)

val next : 'a cursor -> 'a cursor option
(** Moves to the next {e sibling}. *)

val prev : 'a cursor -> 'a cursor option
(** Moves to the previous {e sibling} (reverse of {!next}). *)

val up : 'a cursor -> 'a cursor option
(** Moves to the parent. *)

val down : 'a cursor -> 'a cursor option
(** Moves to the first child.  *)

val dnext : 'a cursor -> 'a cursor option
(** Moves to the next {e node} on the depth-first order enumeration of 
    the complete tree.  *)
val dprev : 'a cursor -> 'a cursor option
(** Moves to the previous {e node} on the depth-first order enumeration of
    the complete tree (reverse of {!dnext}). *)

val find : ('a cursor -> 'a cursor option) -> 
  ('a cursor -> bool) -> 'a cursor option -> 'a cursor option
(** [find mv p c] return the first cursor that satisfies the
    predicate [p] in the cursor enumeration starting with [c] and 
    specified by the move function [mv]. 
    The search stops when [mv] returns [None]. *)

(** {2 Edit} 

    Edits that create two or three adjacent character data siblings
    merge them into a single node [`D]. *)

val set_tree : 'a cursor -> 'a tree -> 'a cursor
(** Sets the cursor to the given tree. *)

val delete : 'a cursor -> 'a cursor
(** Deletes the (tree pointed by the) cursor and moves to the previous
    sibling or up if it doesn't exist. {b Raises} [Invalid_argument] on 
    the root. *)

val insert_below : 'a cursor -> 'a tree -> 'a cursor
(** Inserts a tree before the children of the element pointed by the
    cursor. The new cursor points on the inserted tree. {b Raises}
    [Invalid_argument] on character data. *)

val insert_before : 'a cursor -> 'a tree -> 'a cursor
(** Inserts a tree before the cursor. The new cursor points on the inserted 
    tree. {b Raises} [Invalid_argument] on the root. *)

val insert_after : 'a cursor -> 'a tree -> 'a cursor
(** Inserts a tree after the cursor. The new cursor points on the inserted
    tree. {b Raises} [Invalid_argument] on the root. *)

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
