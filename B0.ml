open B0_kit.V000

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
  B0_ocaml.exe (Fpath.basename ~strip_ext:true src) ~srcs ~doc ~meta ~requires

let test = test_exe "test/test.ml" ~doc:"Test suite"
let test_tree = test_exe "test/test_tree.ml" ~doc:"Test Xmlm.output_tree"
let xhtml = test_exe "test/xhtml.ml" ~doc:"XHTML entities"

let xmltrip =
  let doc = "Reads xml files and outputs them on stdout" in
  let srcs = Fpath.[`File (v "test/xmltrip.ml");
                    `File (v "test/xhtml.ml") ]
  in
  let requires = [xmlm] in
  B0_ocaml.exe "xmltrip" ~public:true ~doc ~srcs ~requires

(* Packs *)

let default =
  let meta =
    B0_meta.empty
    |> B0_meta.(add authors) ["The xmlm programmers"]
    |> B0_meta.(add maintainers)
       ["Daniel Bünzli <daniel.buenzl i@erratique.ch>"]
    |> B0_meta.(add homepage) "https://erratique.ch/software/xmlm"
    |> B0_meta.(add online_doc) "https://erratique.ch/software/xmlm/doc/"
    |> B0_meta.(add licenses) ["ISC"]
    |> B0_meta.(add repo) "git+https://erratique.ch/repos/xmlm.git"
    |> B0_meta.(add issues) "https://github.com/dbuenzli/xmlm/issues"
    |> B0_meta.(add description_tags)
      ["xml"; "codec"; "org:erratique"]
    |> B0_meta.tag B0_opam.tag
    |> B0_meta.add B0_opam.depends
      [ "ocaml", {|>= "4.05.0"|};
        "ocamlfind", {|build|};
        "ocamlbuild", {|build|};
        "topkg", {|build & >= "1.0.3"|};
      ]
    |> B0_meta.add B0_opam.build
      {|[["ocaml" "pkg/pkg.ml" "build" "--dev-pkg" "%{dev}%"]]|}
  in
  B0_pack.make "default" ~doc:"xmlm package" ~meta ~locked:true @@
  B0_unit.list ()
