open B0_kit.V000
open B00_std

(* OCaml library names *)

let xmlm = B0_ocaml.libname "xmlm"

(* Libraries *)

let xmlm_lib =
  let srcs = Fpath.[`Dir (v "src")] in
  let requires = [] in
  B0_ocaml.lib xmlm ~doc:"The xmlm library" ~srcs ~requires

(* Tests *)

let test_exe src ~doc =
  let src = Fpath.v src in
  let srcs = Fpath.[`File src] in
  let meta = B0_meta.(empty |> tag test) in
  let requires = [ xmlm ] in
  B0_ocaml.exe (Fpath.basename ~no_ext:true src) ~srcs ~doc ~meta ~requires

let test = test_exe "test/test.ml" ~doc:"Test suite"
let test_tree = test_exe "test/test_tree.ml" ~doc:"Test Xmlm.output_tree"
let xhtml = test_exe "test/xhtml.ml" ~doc:"XHTML entities"

let xmltrip =
  let doc = "Reads xml files and outputs them on stdout" in
  let srcs = Fpath.[`File (v "test/xmltrip.ml");
                    `File (v "test/xhtml.ml") ]
  in
  let requires = [xmlm] in
  B0_ocaml.exe "xmltrip" ~doc ~srcs ~requires

(* Packs *)

let default =
  let meta =
    let open B0_meta in
    empty
    |> tag B0_opam.tag
    |> add authors ["The xmlm programmers"]
    |> add maintainers ["Daniel Bünzli <daniel.buenzl i@erratique.ch>"]
    |> add homepage "https://erratique.ch/software/xmlm"
    |> add online_doc "https://erratique.ch/software/xmlm/doc/"
    |> add licenses ["ISC"]
    |> add repo "git+https://erratique.ch/repos/xmlm.git"
    |> add issues "https://github.com/dbuenzli/xmlm/issues"
    |> add description_tags
      ["xml"; "codec"; "org:erratique"]
    |> add B0_opam.Meta.depends
      [ "ocaml", {|>= "4.05.0"|};
        "ocamlfind", {|build|};
        "ocamlbuild", {|build|};
        "topkg", {|build & >= "1.0.3"|};
      ]
    |> add B0_opam.Meta.build
      {|[["ocaml" "pkg/pkg.ml" "build" "--dev-pkg" "%{dev}%"]]|}
  in
  B0_pack.v "default" ~doc:"xmlm package" ~meta ~locked:true @@
  B0_unit.list ()
