(*----------------------------------------------------------------------------
   Copyright (c) %%COPYRIGHTYEAR%%, Daniel C. Bünzli. All rights reserved.
   Distributed under a BSD license, see license at the end of the file.
   Xmlm version %%VERSION%%
  ----------------------------------------------------------------------------*)

(** Sequential XML input/output and persistent cursor. 

    The sequential interface reads and writes
    {{:http://www.w3.org/TR/REC-xml}XML} documents in depth-first
    order. Input constructs a value by invoking user provided
    callbacks on the {{:http://www.w3.org/TR/REC-xml/#dt-root} root
    element}. Output proceeds by writing the structure of a root
    element on an output abstraction.

    The {{:#curs}cursor} allows to navigate and update a
    {{:#TYPEtree}simple} in-memory tree representation of the root
    element. Updates performed by the cursor are persistent (non
    destructive).

    Consult the {{:#io}features and limitations} of the 
    input/output functions and {{:#ex}examples} of use.

    {e Version %%VERSION%% {{:mailto:daniel.c.buenzl i\@gmail.com}contact}}.

    {b References.}

    G. Huet. {e {{:http://dx.doi.org/10.1017/S0956796897002864}The Zipper}. 
    J. Functional Programming}, 7 (5), Sept 1997, pp. 549-554.

    Tim Bray. 
    {e {{:http://www.xml.com/axml/axml.html}The annotated XML Specification}}, 
    1998. *)

(** {1 Basic types} *)

(** The type for encodings. *)
type encoding = 
  | UTF_8 
  | UTF_16   
      (** Endianness determined from the 
	  {{:http://www.unicode.org/unicode/faq/utf_bom.html#BOM}BOM}. *)
  | UTF_16BE 
  | UTF_16LE 
  | ISO_8859_1
  | US_ASCII
      
type dtd = string option
(** The type for the optional
    {{:http://www.w3.org/TR/REC-xml/#dt-doctype}DTD}. *)

type name = string * string 
(** The type for attribute and element names. 
    Prefix ({{:http://www.w3.org/TR/REC-xml-names}namespace}, may be empty) 
    and local part. *)

type attribute = name * string
(** The type for attributes. Name and attribute data. *)

type tag = name * attribute list
(** The type for an element tag. Tag name and attribute list. *)

type 'a tree = [ `El of 'a * tag * 'a tree list | `D of string ]
(** The type for trees. Either an element ([`El]) or character data ([`D]). 
    The type ['a] is for user labels. *) 

(** {1 Input} *)

type input
(** The type for input abstractions. *)

val input_of_channel : in_channel -> input
(** Input abstraction from the given channel. *)

val input_of_string : ?pos:int -> string -> input
(** Input abstraction from the given string, starting at [pos], 
    defaults to [0]. *)

val input_of_fun : (unit -> int) -> input
(** Input abstraction from the given function. The function must return
    the next {e byte} as an [int] and raise [End_of_file] if there is no
    such byte. *)

val input : ?enc:encoding option -> ?strip:bool -> 
   ?entity: (string -> string option) ->
   ?prolog: (dtd -> unit) ->
   ?prune:(tag -> 'a -> bool) -> 
   ?d:(string -> 'a -> 'a)  ->
   ?s:(tag -> 'a -> 'a) ->	
   ?e:(tag -> 'a -> 'a) -> 'a -> input -> 'a 
(** Inputs an XML document and invokes callbacks
    on the {{:http://www.w3.org/TR/REC-xml/#dt-root} root element}. 
    The value of type ['a] is passed to 
    the first callback, it is the initial value for the accumulator.
    The function returns the value returned by the last callback.
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
    {- [d], is called on character data, the return value is 
       the accumulator for the next callback. Default returns the accumulator.}
    {- [s], is called whenever an element starts, the return value is
       the accumulator for the next callback. Default returns the accumulator.}
    {- [e], is called whenever an element ends (the given tag is the element's 
       start tag with its attributes), the return value is the accumulator 
       for the next callback. Default returns the accumulator.}}
    
   See an {{:#exintree} example}. 

   {b Raises} {!Error}. *)

val input_tree : ?enc:encoding option -> ?strip:bool -> 
   ?entity: (string -> string option) ->
   ?prolog: (dtd -> unit) ->
   ?prune:(tag -> bool) -> 
   ?d:(string -> string option) -> 
   ?el:(tag -> 'a tree list -> 'a tree option) -> 'a -> input -> 'a tree option
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
        it with the value of type ['a] given to {!input_tree}.}}
    
    {b Raises} {!Error}. *)

(** {2 Input errors} *)

(** The type for input errors. *)
type error = 
  | E_max_buffer_size (** Maximal buffer size exceeded 
			  ([Sys.max_string_length]). *)
  | E_unexpected_eoi (** Unexpected end of input. *)
  | E_malformed_char_stream (** Malformed underlying character stream. *)
  | E_unknown_encoding of string (** Unknown encoding. *)
  | E_unknown_entity_ref of string (** Unknown entity reference, 
				       {{:#input} details}. *)
  | E_illegal_char_ref of string (** Illegal character reference. *)
  | E_illegal_char_seq of string (** Illegal character sequence. *)
  | E_expected_char_seqs of string list * string
    (** Expected one of the character sequences in the list 
	but found another. *)
  | E_expected_root_element (** Expected the document's root element. *)

val error_message : error -> string
(** Converts the error to an english error message. *)

exception Error of (int * int) * error
(** Raised on input errors, reports the line and column number (both 
    start with [1]). *)

(** {1 Output} *)

type output
(** The type for output abstractions. *)

(** In the functions below, [indent] defaults to [None]. Indentation
{{:#output} details}.  *)

val output_of_channel : ?indent:int option -> out_channel -> output
(** Output abstraction from the given channel. *)

val output_of_buffer : ?indent:int option -> Buffer.t -> output
(** Output abstraction from the given buffer. *)

val output_of_fun : ?indent:int option -> (int -> unit) -> output
(** Output abstraction from the given function. The function is
    called with the output {e bytes} as [int]s. *)

val output_prolog : output -> dtd -> unit
(** Outputs a document {{:http://www.w3.org/TR/REC-xml/#NT-prolog}prolog}. 
    You {e should} call this function even if you don't have a DTD. *)

type signal = [ `S of tag | `D of string | `E ]
(** The type for signals. Signals are output fragments, they denote
    either the start of an element ([`S]) or the end of an element
    ([`E]) or character data ([`D]). *)


val output_signal : output -> signal -> unit
(** Outputs a signal. See {{:#exseq} exa}{{:#exouttree}mples}. 

    {b Raises} [Invalid_argument] if a signal [`D]
    is output outside any open element or if an output [`E] has no matching
    [`S]. *)

val output_finish : ?nl:bool -> output -> unit
(**  Must be called to finish output. If [nl] is [true] a newline
    character ['\n'] is output, defaults to [false]. Note that for outputs on
    channels this does not flush the buffer.  After the call, the
    output abstraction can be used to output a new document.

    {b Raises.} [Invalid_argument] if an [`S] was output without
     a matching [`E]. *)

val output_tree : ?t:('a tree -> 'a tree option) -> output -> 'a tree -> unit
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

(** {1:io Features and limitations}
    
    The module assumes strings are immutable, thus strings
    you give or receive {e during} input and output must not
    be modified.
    
    {4:input Input}
    {ul    
    {- Encoding. The parser supports ASCII, US-ASCII, 
    {{:http://www.faqs.org/rfcs/rfc3629.html} UTF-8},
    {{:http://www.faqs.org/rfcs/rfc2781.html} UTF-16},
    {{:http://www.faqs.org/rfcs/rfc2781.html} UTF-16LE},
    {{:http://www.faqs.org/rfcs/rfc2781.html} UTF-16BE} and
    {{:http://anubis.dkuug.dk/JTC1/SC2/WG3/docs/n411.pdf}ISO-8559-1} 
    (Latin-1) encoded documents. But strings returned by
    the library are {b always} UTF-8 encoded. 
    
    The encoding can be specified
    explicitely using the optional argument [enc]. Otherwise the parser
    uses the encoding specified in the 
    {{:http://www.w3.org/TR/REC-xml/#NT-XMLDecl} XML declaration}.
    If there is no such declaration,
    UTF-16 is used provided there is an UTF-16 encoded 
    {{:http://www.unicode.org/unicode/faq/utf_bom.html#BOM}BOM} at the
    beginning of the document. Otherwise UTF-8 is assumed. }
    {- White space handling (mon Dieu). 
    
       Attribute data. The parser performs
       {{:http://www.w3.org/TR/REC-xml/#AVNormalize}attribute data
       normalization} on {e every} attribute data.  This means that
       attribute data does not have leading and trailling white space and that 
       any white space is collapsed and transformed to a single [' '] space 
       character.

       Character data. White space handling of character data depends
       on the [strip] argument. If [strip] is [true],
       character data is treated like attribute data, white space before
       and after elements is removed and any white space is collapsed and
       transformed to a single [' '] space character. 
       If [strip] is [false] (default) then all white
       space data is preserved as present in the document (however all
       kinds of 
       {{:http://www.w3.org/TR/REC-xml/#sec-line-ends}line ends} are
       translated to the single character ['\n']). }
    {- Entity references.
    {{:http://www.w3.org/TR/REC-xml/#dt-charref}Character references} and
    {{:http://www.w3.org/TR/REC-xml/#sec-predefined-ent}predefined
    entities} are automatically resolved. Other entity references
    can be resolved by the callback [entity], which
    must return an UTF-8 string corresponding to the replacement character data
    (the replacement data is {e not} analysed for further references, it is
     added to the data as such modulo white space stripping). If
    [entity] returns [None] the error [E_unknown_entity_ref] is raised.}
    {- Pruning. Elements can be pruned at parse time by using the [prune]
    callback. To understand how this interacts with white space handling, 
    think about pruning as if characters from the starting ['<'] of
    the element until the closing ['>'] are deleted from the document and
    the result is parsed. Note that in pruned elements, 
    attribute and character data parsing is rough. 
    This means that non well-formed documents
    can be read without producing errors (e.g. presence of a ['<'] in 
    attribute data of a pruned element).}    
    {- Parsing ends when the root element is closed. No data is consumed
       beyond the closing ['>'] of the root element. If you expect another
       document after, you can continue to input with the same input 
       abstraction.}
    {- Parses qualified names 
    ({{:http://www.w3.org/TR/REC-xml-names}namespaces}).}
    {- Parses the more liberal and simpler XML 1.1 
    {{:http://www.w3.org/TR/xml11/#NT-Name}Name} definition (minus [':'] because
    of namespaces).}
    {- The {{:http://www.w3.org/TR/REC-xml/#dt-doctype}DTD} is parsed
      roughly (no guarantee it is well formed) and its information is ignored.}
    {- The parser drops 
    {{:http://www.w3.org/TR/REC-xml/#dt-comment}comments}, 
    {{:http://www.w3.org/TR/REC-xml/#dt-pi}processing instructions}, and 
    {{:http://www.w3.org/TR/REC-xml/#sec-rmd}standalone declaration}.}
    {- Attribute and character data chunks are limited by 
       [Sys.max_string_length]. An {!error} is raised if the limit is hit.}
    {- Tail recursive.}
    {- Non validating.}
    }
    
    {4:output Output}
    {ul
    {- Encoding. Outputs 
        only {{:http://www.faqs.org/rfcs/rfc3629.html} UTF-8}
       encoded documents. Strings given to output functions {b must be} 
       UTF-8 encoded, no checks are performed.}
    {- Indentation. Output can be indented by specifying the [indent]
       argument when an output abstraction is created. If [indent] is [None] 
       (default) signal output does not introduce any extra white space.
       If [ident] is [Some c], each {!signal}
       is output on its own line (for empty elements [`S] 
       and [`E] are collapsed on a single line)
       and nested elements are indented with [c] space characters.}
    {- In attribute and character data you provide, markup 
       delimiters ['<'],['>'],['&'], and ['\"'] are 
        automatically escaped to 
        {{:http://www.w3.org/TR/REC-xml/#sec-predefined-ent}predefined
        entities}. Hence for character and attribute data you don't need
        to bother about what you give to output as long as it is 
        UTF-8 encoded.}
    {- No checks are peformed on the prefix and local part of 
      {!name}s to verify they are
      {{:http://www.w3.org/TR/REC-xml-names/#NT-NCName}NCName}s.
      For example using the tag name [("","dip d")] will produce 
      a non well-formed document because of the space character.}
    {- Tail recursive.}}

    {4 Tips} 
    {ul 
    {- The best options to do an input/output round trip
       and preserve as much information as possible is to 
       input with [strip = false] and output with [indent = None].}
    {- If you don't need all information in the document, parsing 
       performance can be improved on large inputs by pruning with [prune]. 
       Once an element gets pruned, its subtree is parsed but no attribute and 
       character data is buffered.}}
*)

(** {1:ex Examples} 

    {4:exseq Sequential processing}    

    This example shows how to process documents without building an
    in-memory representation. The function [prune_in] prunes 
    a document from elements belonging to a [prune_list] and outputs
    the result.  The tree is
    pruned with the [prune] callback which takes a tag and returns
    [true] if the tag name is in the prune list. This disables
    callbacks for the element and its descendents. The other callbacks [d], [s] 
    and [e] just signal the corresponding event --- character data, start of an 
    element, end of an element --- on the output abstraction.
{[let prune_in prune_list ic oc = 
  let i = Xmlm.input_of_channel ic in
  let o = Xmlm.output_of_channel oc in
  let prolog = Xmlm.output_prolog o in
  let prune (name, _) _ = List.mem name prune_list in 
  let d data _ = Xmlm.output_signal o (`D data) in 
  let s tag _ = Xmlm.output_signal o (`S tag) in
  let e _ _ = Xmlm.output_signal o `E in
  Xmlm.input ~prune ~prolog ~d ~s ~e () i;
  Xmlm.output_finish o]}

    {4:exintree Custom tree input} 
    
    To build a tree you can directly use {!input_tree}. However you may
    need to interface with a different tree data structure. This can 
    be done with {!input}. Assume your trees are defined as 
    follows.
{[type t = El of Xmlm.tag * t list | D of string ]}

    The accumulator ([path]) given to {!input} is a stack of lists of
    type [t] representing ancestor elements whose parsing needs to be
    finished --- more children need to be parsed. The top of the stack
    holds the children of the element being parsed. When a new element
    starts, [s] is invoked, it pushes an empty list of children on the
    stack. When an element ends, [e] is invoked, it constructs the
    value [el] for the element with the children on top of the stack,
    pops the stack, and adds [el] to the parent's children ([parent]).
    The function [d] is called on character data, it just adds a new
    child in the list of children on top of the stack.
{[let in_tree ic =
  let i = Xmlm.input_of_channel ic in
  let d data = function 
    | childs :: path -> ((D data) :: childs) :: path 
    | [] -> assert false
  in
  let s _ path = [] :: path in
  let e tag = function
    | childs :: path -> 
	let el = El (tag, List.rev childs) in
	begin match path with
	| parent :: path' -> (el :: parent) :: path' 
	| [] -> [ [ el ] ]
	end
    | [] -> assert false
  in
  match Xmlm.input ~d ~s ~e [] i with
  | [ [ root ] ] -> root
  | _ -> assert false (* cannot happen (no pruning) *)
]}

  {4:exouttree Custom tree output} 

  This example shows how to output the tree representation of the last
  section. Care must be taken to make the function tail recursive. The
  internal function [aux] acts on a stack of lists of type [t]
  ([path]) representing ancestor elements whose output needs to be finished
  --- more children need to be output. When an element is
  deconstructed a start element is signaled and its children are
  pushed on the stack. When the list on top of the stack becomes
  empty, all the children of the element were output and we can signal
  the end of an element.  Character data is signaled with a [`D] and
  removed from the list of children on top of the stack. [aux] stops
  when the last list of children becomes empty.
{[let out_tree oc t = 
  let o = Xmlm.output_of_channel oc in 
  let rec aux o = function
    | (n :: next) :: path -> 
      begin match n with
      | El (tag, childs) -> 
	  Xmlm.output_signal o (`S tag); 
	  aux o (childs :: next :: path)
      | D d -> 
	  Xmlm.output_signal o (`D d);
	  aux o (next :: path)
      end
    | [] :: [] -> ()
    | [] :: path -> Xmlm.output_signal o `E; aux o path
    | [] -> assert false
  in
  Xmlm.output_prolog o None;
  aux o [ [ t ] ];
  Xmlm.output_finish o]}
*) 

(**/**)
val add_uchar : Buffer.t -> int -> unit
(**/**)

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
