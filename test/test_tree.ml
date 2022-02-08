let li d = `El ((("", "li"), []), [`Data d])
let frag = `El ((("", "ol"), []), [li "bli"; li "bla"; li "blo"])

let main () =
  let b = Buffer.create 233 in
  let o = Xmlm.make_output (`Buffer b) in
  Xmlm.output o (`Dtd None);
  Xmlm.output_tree (fun x -> x) o frag;
  print_endline (Buffer.contents b)

let () = main ()
