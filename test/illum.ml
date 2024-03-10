open IllumLib.Utils
open IllumLib.Nf

let home = "/home/bart/progs/ocaml/illum-lang/test/"

let read_file filename =
  let ch = open_in (home ^ filename) in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch; s

(******************************************************************************)
(*                                       NF1                                  *)
(******************************************************************************)

let test_is_nf1 b fname = match read_file fname with
      "" -> false
    | s -> s |> parse |> nf0 |> is_nf1 |> (fun x -> x = b)

let test_nf1 fname = match read_file fname with
    "" -> false
  | s -> s |> parse |> nf0 |> nf1 |> is_nf1

let%test "is_nf1_test0" = test_is_nf1 true "nf1/test0.hll"
let%test "is_nf1_test1" = test_is_nf1 false "nf1/test1.hll"

let%test "nf1_test0" = test_nf1 "nf1/test0.hll"
let%test "nf1_test1" = test_nf1 "nf1/test1.hll"
(* let%test "nf1_test2" = test_nf1 "nf1/test2.hll" *)
let%test "nf1_test3" = test_nf1 "nf1/test3.hll"
let%test "nf1_test4" = test_nf1 "nf1/test4.hll"
let%test "nf1_test5" = test_nf1 "nf1/test5.hll"
let%test "nf1_test6" = test_nf1 "nf1/test6.hll"
let%test "nf1_test7" = test_nf1 "nf1/test7.hll"
let%test "nf1_test8" = test_nf1 "nf1/test8.hll"
let%test "nf1_test9" = test_nf1 "nf1/test9.hll"


(******************************************************************************)
(*                                       NF2                                  *)
(******************************************************************************)

let test_nf2 fname = match read_file fname with
  "" -> false
| s -> s |> parse |> nf0 |> nf1 |> nf2 |> is_nf2

let%test "nf2_test0" = test_nf2 "nf2/test0.hll"
let%test "nf2_test1" = test_nf2 "nf2/test1.hll"
