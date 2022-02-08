#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "xmlm" @@ fun c ->
  Ok [ Pkg.mllib "src/xmlm.mllib";
       Pkg.bin "test/xmltrip";
       Pkg.test "test/examples";
       Pkg.test "test/test";
       Pkg.test "test/xhtml";
       Pkg.test "test/test_tree";
       Pkg.doc "doc/index.mld" ~dst:"odoc-pages/index.mld";
       Pkg.doc "test/examples.ml";
       Pkg.doc "test/xhtml.ml"; ]
