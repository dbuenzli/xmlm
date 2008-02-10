(*----------------------------------------------------------------------------
   Copyright (c) %%COPYRIGHTYEAR%%, Daniel C. Bünzli. All rights reserved.
   Distributed under a BSD license, see license at the end of the file.
   Xmlm version %%VERSION%%
  ----------------------------------------------------------------------------*)

(** Sequential XML IO. 
    
    Xmlm inputs and outputs {{:http://www.w3.org/TR/REC-xml}XML}
    documents sequentially. Input constructs a custom value by
    invoking client provided callbacks on the
    {{:http://www.w3.org/TR/REC-xml/#dt-root} root element} in depth first 
    order. 
    Output proceeds by writing in depth first order the
    structure of a root element on an output abstraction.

    Consult the {{:#io}features and limitations} and {{:#ex}examples} 
    of use.

    {b References.}

    Tim Bray. 
    {e {{:http://www.xml.com/axml/axml.html}The annotated XML Specification}}, 
    1998. 

    Tim Bray et al. 
    {e {{:http://www.w3.org/TR/xml-names11}Namespaces in XML 1.1 (2nd ed.)}},
    2006.

    {b Version} %%VERSION%%, %%EMAIL%%
    {1 Basic types and values} *)

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
(** The type for attribute and element's
    {{:http://www.w3.org/TR/xml-names11/#dt-expname}expanded names} 
    [(uri,local)]. An empty [uri] represents a name without a
    namespace name, i.e. an unprefixed name 
    that is not under the scope of a default namespace. *)

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
				      {{:#inentity} details}. *)
  | `Unknown_ns_prefix of string (** Unknown namespace prefix 
				     {{:#inns} details} *)
  | `Illegal_char_ref of string (** Illegal character reference. *)
  | `Illegal_char_seq of string (** Illegal character sequence. *)
  | `Expected_char_seqs of string list * string
	(** Expected one of the character sequences in the list 
	    but found another. *)
  | `Expected_root_element (** Expected the document's root element. *) ]

val error_message : error -> string
(** Converts the error to an english error message. *)


val ns_xml : string 
(** Namespace name {{:http://www.w3.org/XML/1998/namespace}value} bound to the 
    reserved ["xml"] prefix. *)

val ns_xmlns : string
(** Namespace name {{:http://www.w3.org/2000/xmlns/}value} bound to the 
    reserved ["xmlns"] prefix. *)

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
   ?ns: (string -> string option) ->
   ?entity: (string -> string option) ->
   ?prolog: (dtd -> unit) ->
   ?prune:(tag -> 'a -> bool) -> 
   ?s:(tag -> 'a -> 'a) ->	
   ?e:(tag -> 'a -> 'a) -> 
   ?d:(string -> 'a -> 'a)  -> 'a -> input -> 
     [ `Success of 'a | `Error of (int * int) * error ]
(** Inputs an XML document and invokes callbacks on the
    {{:http://www.w3.org/TR/REC-xml/#dt-root} root element}.  The
    value of type ['a] is passed to the first callback, it is the
    initial value for the accumulator.  The function returns [`Value]
    with the value returned by the last callback or an [`Error] with the
    [(line, column)] numbers (both start with [1]) and the cause. 
    {ul 
    {- [enc], character encoding of the document, {{:#inenc} details}. 
       Defaults to [None].}
    {- [strip], strips whitespace in character data, {{:#inwspace} details}.
       Defaults to [false].} 
    {- [ns] is called to bind undeclared namespace prefixes,
       {{:#inns} details}. Default returns always [None].}
    {- [entity] is called to resolve non predefined entity references,
       {{:#inentity} details}. Default returns always [None].}
    {- [prune] is called whenever a new element starts. If it returns [true],
       no callbacks are invoked for the element and its children, 
       {{:#inprune} details}. Default returns always [false].}
    {- [prolog] is called with the optional DTD just after the document
       {{:http://www.w3.org/TR/REC-xml/#NT-prolog}prolog} was parsed.
       Default does nothing.}
    {- [s], is called whenever an element starts, the return value is
       the accumulator for the next callback. Default returns the accumulator.}
    {- [e], is called whenever an element ends (the given tag is the element's 
       start tag with its attributes), the return value is the accumulator 
       for the next callback. Default returns the accumulator.}
    {- [d], is called on character data, the return value is 
       the accumulator for the next callback. Default returns the accumulator.
       The module guarantees that this function won't be called twice 
       consecutively.}}
    
   See {{:#ex} examples}. *)

val input_tree : ?enc:encoding option -> ?strip:bool -> 
   ?ns: (string -> string option) ->
   ?entity: (string -> string option) ->
   ?prolog: (dtd -> unit) ->
   ?prune:(tag -> bool) -> 
   el:(tag -> 'a list -> 'a) -> 
   d:(string -> 'a)  -> input -> 
     [ `Success of 'a option | `Error of (int * int) * error ]
(** See {!input}. This function facilitates the construction 
    of tree datastructures via the [el] and [d] callbacks. [None] is
    returned in case of success if the root element was pruned.
    {ul 
    {- [el], is called after an element was parsed with its tag and 
    children.} 
    {- [d], is called on character data. The module guarantees that this
     function won't be called twice consecutively.}} 

    See an {{:#extree} example}.*)

(** {1 Output} *)

type output
(** The type for output abstractions. *)

type signal = [ `S of tag | `D of string | `E ]
(** The type for signals. Signals are output fragments, they denote
    either the start of an element ([`S]) or the end of an element
    ([`E]) or character data ([`D]). *)

type 'a tree = [ `El of tag * 'a list | `D of string ]
(** The type for deconstructing trees. *)

(** In the functions below, 
    {ul 
    {- [prefix] defeault always returns [None].
    See namespace {{:#outns}details}.}
    {- [indent] defaults to [None]. See indentation {{:#outindent} details.}}}*)


val output_of_channel : ?indent:int option -> 
  ?prefix:(string -> string option) -> out_channel -> output
(** Output abstraction from the given channel. *)

val output_of_buffer : ?indent:int option -> 
  ?prefix:(string -> string option) -> Buffer.t -> output
(** Output abstraction from the given buffer. *)

val output_of_fun : ?indent:int option -> 
  ?prefix:(string -> string option) -> (int -> unit) -> output
(** Output abstraction from the given function. The function is
    called with the output {e bytes} as [int]s. *)

val output_prolog : output -> dtd -> unit
(** Outputs a document {{:http://www.w3.org/TR/REC-xml/#NT-prolog}prolog}. 
    You {e should} call this function even if you don't have a DTD. *)

val output_signal : output -> signal -> unit
(** Outputs a signal. See {{:#ex}examples}. 

    {b Raises} [Invalid_argument] if a signal [`D]
    is output outside any open element or if an output [`E] has no matching
    [`S] or if a namespace name could not be bound to a prefix. *)

val output_tree : ('a -> 'a tree) -> output -> 'a -> unit
(** Outputs signals corresponding to the given tree by using
    the given tree deconstructor. See an {{:#extree} example}. *)
   
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
    {3:inenc Encoding}    

    The parser supports ASCII, US-ASCII, 
    {{:http://www.faqs.org/rfcs/rfc3629.html} UTF-8},
    {{:http://www.faqs.org/rfcs/rfc2781.html} UTF-16},
    {{:http://www.faqs.org/rfcs/rfc2781.html} UTF-16LE},
    {{:http://www.faqs.org/rfcs/rfc2781.html} UTF-16BE} and
    {{:http://anubis.dkuug.dk/JTC1/SC2/WG3/docs/n411.pdf}ISO-8559-1} 
    (Latin-1) encoded documents. But strings returned by
    the library are {b always} UTF-8 encoded. 
    
    The encoding can be specified explicitly using the optional
    argument [enc]. Otherwise the parser uses the encoding specified
    in the {{:http://www.w3.org/TR/REC-xml/#NT-XMLDecl} XML
    declaration}.  If there is no such declaration, UTF-16 is used
    provided there is an UTF-16 encoded
    {{:http://www.unicode.org/unicode/faq/utf_bom.html#BOM}BOM} at the
    beginning of the document. Otherwise UTF-8 is assumed.
    {3:inwspace White space handling}

    The parser performs
    {{:http://www.w3.org/TR/REC-xml/#AVNormalize}attribute data
    normalization} on {e every} attribute data.  This means that
    attribute data does not have leading and trailling white space and that 
    any white space is collapsed and transformed to a single [' '] space 
    character.

    White space handling of character data depends on the [strip]
    argument. If [strip] is [true], character data is treated like
    attribute data, white space before and after elements is removed
    and any white space is collapsed and transformed to a single [' ']
    space character.  If [strip] is [false] (default) then all white
    space data is preserved as present in the document (however all
    kinds of {{:http://www.w3.org/TR/REC-xml/#sec-line-ends}line ends}
    are translated to the single character ['\n']).  
    {3:inns Namespaces}

    Names passed to callbacks are 
    {{:http://www.w3.org/TR/xml-names11/#dt-expname}expanded names}.

    The parser automatically handles the document's namespace
    declarations.  Undeclared namespace prefixes can be bound via the
    callback [ns], which must return a namespace name. If [ns] returns
    [None] an [`Unknown_ns_prefix] error is returned.

    Attributes used for namespace declarations are still passed to the
    callbacks. They are in the {!ns_xmlns} namespace. Default
    namespace declarations made with {i xmlns} have the attribute name
    [(Xmlm.ns_xmlns, "xmlns")]. Prefix declarations have the prefix as
    the local name, for example {i xmlns:ex} results in
    [(Xmlm.ns_xmlns, "ex")].

    Regarding constraints on the usage of the {i xml} and {i xmlns}
    prefixes by documents, the parser does not report errors on violations 
    of the {i must} constraints listed in
    {{:http://www.w3.org/TR/xml-names11/#xmlReserved}this paragraph}. 
    {3:inentity Entity references}

    {{:http://www.w3.org/TR/REC-xml/#dt-charref}Character references} and
    {{:http://www.w3.org/TR/REC-xml/#sec-predefined-ent}predefined
    entities} are automatically resolved. Other entity references
    can be resolved by the callback [entity], which
    must return an UTF-8 string corresponding to the replacement character data.
    The replacement data is {e not} analysed for further references, it is
     added to the data as such modulo white space stripping. If
    [entity] returns [None] the error [`Unknown_entity_ref] is returned.
    {3:inprune Pruning} 

    Elements can be pruned at parse time by using
    the [prune] callback. To understand how this interacts with white
    space handling, think about pruning as if characters from the
    starting ['<'] of the element until the closing ['>'] are deleted
    from the document and the result is parsed. Note that in pruned
    elements, attribute and character data parsing is rough.  This
    means that non well-formed documents can be read without producing
    errors (e.g. presence of a ['<'] in attribute data of a pruned
    element).
    {3:inmisc Miscellaneous}
    {ul
    {- Parsing ends when the root element is closed. No data is consumed
       beyond the closing ['>'] of the root element. If you expect another
       document after, you can continue to input with the same input 
       abstraction.}
    {- Parses the more liberal and simpler XML 1.1 
    {{:http://www.w3.org/TR/xml11/#NT-Name}Name} definition (minus [':'] because
    of namespaces).}
    {- The {{:http://www.w3.org/TR/REC-xml/#dt-doctype}DTD} is parsed
      roughly (no guarantee it is well formed) and its information is ignored.}
    {- The parser drops 
    {{:http://www.w3.org/TR/REC-xml/#dt-comment}comments}, 
    {{:http://www.w3.org/TR/REC-xml/#dt-pi}processing instructions}, and 
    {{:http://www.w3.org/TR/REC-xml/#sec-rmd}standalone declaration}.}
    {- Element attribute names are not checked for uniqueness.}
    {- Attribute and character data chunks are limited by 
       [Sys.max_string_length]. The error [`Max_buffer_size] is returned if the 
    limit is hit.}
    {- Tail recursive.}
    {- Non validating.}
    }
    

    {2:output Output} 
    {3:outenc Encoding} 

    Outputs only {{:http://www.faqs.org/rfcs/rfc3629.html} UTF-8}
    encoded documents. Strings given to output functions {b must be}
    UTF-8 encoded, no checks are performed.
    {3:outns Namespaces}

    The type of names expected by output functions depends on the
    [names] argument. With [`Qname] they are
    {{:http://www.w3.org/TR/xml-names11/#dt-qualname}qualified
    names}.  With [`Ename] they are
    {{:http://www.w3.org/TR/xml-names11/#dt-expname}expanded names}.


    Expanded names are automatically converted to
    {{:http://www.w3.org/TR/xml-names11/#dt-qualname}qualified
    names} by the module. There is no particular api to specify 
    prefixes and default namespaces, 
    the actual result depends solely on the output
    of attributes belonging to the {!ns_xmlns} namespace. For example to set 
    the default namespace of an element to {i http://example.org/myns}, 
    use the following attribute :
    {[(* xmlns='http://example.org/myns' *)
let default_ns = (Xmlm.ns_xmlns, "xmlns"), "http://example.org/myns"]}
    To bind the prefix ["ex"] to {i http://example.org/ex}, use the 
    following attribute :
    {[(* xmlns:ex='http://example.org/ex' *)
let ex_ns = (Xmlm.ns_xmlns, "ex"), "http://example.org/ex"]}
    Note that chaining input with expanded names to output 
    with expanded names without touching namespace declaration 
    attributes will preserve existing prefixes and bindings provided
    the same namespace name is not bound to different prefixes in
    a given context.

    The optional function given with [`Ename] is used to give a prefix
    to a namespace name lacking a prefix binding in the current output
    scope. Given the namespace name without prefix binding, the function
    must return a prefix to use. Note that this will {b not} add any namespace 
    declaration attribute to the output.
    If the function returns [None], {!output_signal} 
    will raise [Invalid_argument].  The default function returns always [None].
    
    {3:outindent Indentation}

    Output can be indented by specifying the [indent] argument when an
       output abstraction is created. If [indent] is [None] (default)
       signal output does not introduce any extra white space.  If
       [ident] is [Some c], each {!signal} is output on its own line
       (for empty elements [`S] and [`E] are collapsed on a single
       line) and nested elements are indented with [c] space
       characters.
    {3:outmisc Miscellaneous}
    {ul
    {- In attribute and character data you provide, markup 
       delimiters ['<'],['>'],['&'], and ['\"'] are 
        automatically escaped to 
        {{:http://www.w3.org/TR/REC-xml/#sec-predefined-ent}predefined
        entities}. Hence for character and attribute data you don't need
        to bother about what you give to output as long as it is 
        UTF-8 encoded.}
    {- No checks are peformed on the prefix and local part of 
      {!name}s to verify they are
      {{:http://www.w3.org/TR/xml-names11/#NT-NCName}NCName}s.
      For example using the tag name [("","dip d")] will produce 
      a non well-formed document because of the space character.}
    {- Tail recursive.}}

    {2 Tips} 
    {ul 
    {- The best options to do an input/output round trip
       and preserve as much information as possible is to 
       input with [strip = false] and output with [indent = None].}
    {- Performance. 

       If you don't need all information in the document, parsing 
       performance can be improved on large inputs by pruning with [prune]. 
       Once an element gets pruned, its subtree is parsed but no attribute and 
       character data is buffered.

       IO with qualified names is faster.}} 
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
  let s tag _ = Xmlm.output_signal o (`S tag) in
  let e _ _ = Xmlm.output_signal o `E in
  let d data _ = Xmlm.output_signal o (`D data) in 
  Xmlm.input ~prune ~prolog ~s ~e ~d () i;
  Xmlm.output_finish o]}

    {2:extree Tree processing} 
    
    Trees can be easily input and output with {!input_tree} and 
    {!output_tree}.

    Assume your trees are defined as follows.
    {[type tree = El of Xmlm.tag * tree list | D of string]}
    The following function builds a value of type [tree] on input. 
{[let input_tree = 
  let el tag childs = El (tag, childs) in
  let d data = D data in
  Xmlm.input_tree ~el ~d]}
    The following function outputs signals corresponding to 
    the given tree.
{[let output_tree = 
  let fold = function 
    | El (tag, childs) -> `El (tag, childs) 
    | D d -> `D d 
  in
  Xmlm.output_tree fold]}
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
