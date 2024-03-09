open IllumLib.Utils
open IllumLib.Nf

let home = "/home/bart/progs/ocaml/illum-lang/test/"

let read_file filename =
  let ch = open_in (home ^ filename) in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch; s

let test_is_nf1 fname = match read_file fname with
      "" -> false
    | s -> s |> parse |> nf0 |> is_nf1

let%test "test0_nf1" = test_is_nf1 "nf1/test0.hll"
