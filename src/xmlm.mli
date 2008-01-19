(*----------------------------------------------------------------------------
   Copyright (c) %%COPYRIGHTYEAR%%, Daniel C. Bünzli. All rights reserved.
   Distributed under a BSD license, see license at the end of the file.
   Xmlm version %%VERSION%%
  ----------------------------------------------------------------------------*)

(** XML input/output. 

    
    Xmlm reads and writes {{:http://www.w3.org/TR/REC-xml}XML}
    documents sequentially. Input constructs a custom value by
    invoking user provided callbacks on the
    {{:http://www.w3.org/TR/REC-xml/#dt-root} root element} in
    depth-first order. Output proceeds by writing the
    structure of a root element on an output abstraction.

    Consult the {{:#io}features and limitations} and {{:#ex}examples} 
    of use.

    {b References.}

    Tim Bray. 
    {e {{:http://www.xml.com/axml/axml.html}The annotated XML Specification}}, 
    1998. 

    Tim Bray et al. 
    {e {{:http://www.w3.org/TR/REC-xml-names}Namespaces in XML 1.0 (2nd ed.)}},
    2006.

    {b Version} %%VERSION%%, %%EMAIL%%
    {1 Basic types} *)

(** The type for character encodings. For [`UTF_16],
    endianness is determined from the 
	  {{:http://www.unicode.org/unicode/faq/utf_bom.html#BOM}BOM}. *) 
type encoding = [ 
  | `UTF_8 
  | `UTF_16   
      (** Endianness determined from the 
	  {{:http://www.unicode.org/unicode/faq/utf_bom.html#BOM}BOM}. *)
  | `UTF_16BE 
  | `UTF_16LE 
  | `ISO_8859_1
  | `US_ASCII ]
      
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

(** The type for input errors. *)
type error = [
  | `Max_buffer_size (** Maximal buffer size exceeded 
			 ([Sys.max_string_length]). *)
  | `Unexpected_eoi (** Unexpected end of input. *)
  | `Malformed_char_stream (** Malformed underlying character stream. *)
  | `Unknown_encoding of string (** Unknown encoding. *)
  | `Unknown_entity_ref of string (** Unknown entity reference, 
				      {{:#input} details}. *)
  | `Illegal_char_ref of string (** Illegal character reference. *)
  | `Illegal_char_seq of string (** Illegal character sequence. *)
  | `Expected_char_seqs of string list * string
	(** Expected one of the character sequences in the list 
	    but found another. *)
  | `Expected_root_element (** Expected the document's root element. *) ]

val error_message : error -> string
(** Converts the error to an english error message. *)

(**/**)
val add_uchar : Buffer.t -> int -> unit
(**/**)


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
   ?e:(tag -> 'a -> 'a) -> 'a -> input -> 
     [ `Value of 'a | `Error of (int * int) * error ]
(** Inputs an XML document and invokes callbacks on the
    {{:http://www.w3.org/TR/REC-xml/#dt-root} root element}.  The
    value of type ['a] is passed to the first callback, it is the
    initial value for the accumulator.  The function returns [`Value]
    with the value returned by the last callback or an [`Error] with the
    [(line, column)] numbers (both start with [1]) and the cause. 
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
    
   See {{:#ex} examples}. *)

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
(** Outputs a signal. See {{:#ex}examples}. 

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

(** {1:io Features and limitations}
    
    The module assumes strings are immutable, thus strings
    you give or receive {e during} input and output must not
    be modified.
    
    {2:input Input}
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
    
    {2:output Output}
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

    {2 Tips} 
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

    {2:exseq Sequential processing}    

    This example shows how to process documents without building an
    in-memory representation. The function [prune_in] prunes 
    a document from elements belonging to a [prune_list] and outputs
    the result.  

    The tree is pruned with the [prune] callback which takes a tag and
    returns [true] if the tag name is in the prune list. This disables
    callbacks for the element and its descendents. The other callbacks
    [d], [s] and [e] just signal the corresponding event --- character
    data, start of an element, end of an element --- on the output
    abstraction.
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

    {2 Custom tree input/output} 
    
    The following code can be found in the [test/tree.ml] file 
    of the distribution.
    
    Assume your trees are defined as follows.
    {[type t = [ `El of Xmlm.tag * t list | `D of string ]]}
    To construct the tree, the accumulator ([path]) given to {!input}
    is a stack of lists of type [t] representing ancestor elements
    whose parsing needs to be finished, more children need to be
    parsed. The top of the stack holds the children of the element
    being parsed. When a new element starts, [s] is invoked, it pushes
    an empty list of children on the stack. When an element ends, [e]
    is invoked, it constructs the value [el] for the element with the
    children on top of the stack, pops the stack, and adds [el] to the
    parent's children ([parent]).  The function [d] is called on
    character data, it just adds a new child in the list of children
    on top of the stack.
{[let input ?enc ?strip ?entity ?prolog ?prune i = 
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
  | `Value [ [ root ] ] -> `Value (Some root)
  | `Value [ [] ]  -> `Value None (* the root was pruned *)
  | `Error _ as e -> e
  | _ -> assert false
]}

  The function below outputs signals corresponding to the structure of
  the given tree. Care must be taken to make the function tail
  recursive. The internal function [aux] matches on a stack of lists
  of type [t] representing ancestor elements whose output needs to be
  finished, more children need to be output.
  {ol 
  {- If the list on the top of the stack is not empty we deconstruct it.
    {ol
    {- If the head is an element [`El], it is signaled with a [`S],
       removed from the list, and we push its children of on the
       stack. These need to be output now, before the rest.}
    {- If the head is character data [`D], it is signaled with a [`D]
       and removed from the list of children on top of the stack.}}}
  {- If the list on the top of the stack is empty and the stack is empty,
     we have output everything. We can stop.}
  {- If the list on the top of the stack is empty but the stack is not,
     we finished to output the children of an element. 
     Thus we signal the end of an element with an [`E] and pop the stack.}}

 We start [aux] with the root element on the stack. 
{[let output o t = 
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
]}
*) 

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
