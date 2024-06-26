open IllumLib.Utils
open IllumLib.Nf
open IllumLib.Hllc

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
let%test "nf1_test2" = test_nf1 "nf1/test2.hll"
let%test "nf1_test3" = test_nf1 "nf1/test3.hll"
let%test "nf1_test4" = test_nf1 "nf1/test4.hll"
let%test "nf1_test5" = test_nf1 "nf1/test5.hll"
let%test "nf1_test6" = test_nf1 "nf1/test6.hll"
let%test "nf1_test7" = test_nf1 "nf1/test7.hll"
let%test "nf1_test8" = test_nf1 "nf1/test8.hll"
let%test "nf1_test9" = test_nf1 "nf1/test9.hll"
let%test "nf1_test10" = test_nf1 "nf1/test10.hll"
let%test "nf1_test11" = test_nf1 "nf1/test11.hll"
let%test "nf1_test12" = test_nf1 "nf1/test12.hll"
let%test "nf1_test13" = test_nf1 "nf1/test13.hll"
let%test "nf1_test14" = test_nf1 "nf1/test14.hll"
let%test "nf1_test15" = test_nf1 "nf1/test15.hll"
let%test "nf1_test16" = test_nf1 "nf1/test16.hll"

(******************************************************************************)
(*                                       NF2                                  *)
(******************************************************************************)

let test_nf2 fname = match read_file fname with
  "" -> false
| s -> s |> parse |> nf0 |> nf1 |> nf2 |> is_nf2

let%test "nf2_test0" = test_nf2 "nf2/test0.hll"
let%test "nf2_test1" = test_nf2 "nf2/test1.hll"
let%test "nf2_test2" = test_nf2 "nf2/test2.hll"
let%test "nf2_test3" = test_nf2 "nf2/test3.hll"
let%test "nf2_test4" = test_nf2 "nf2/test4.hll"
let%test "nf2_test5" = test_nf2 "nf2/test5.hll"

(******************************************************************************)
(*                                       NF3                                  *)
(******************************************************************************)

let test_nf3 fname = match read_file fname with
  "" -> false
| s -> s |> parse |> nf0 |> nf1 |> nf2 |> nf3 |> is_nf3

let%test "nf3_test0" = test_nf3 "nf3/test0.hll"
let%test "nf3_test1" = test_nf3 "nf3/test1.hll"
let%test "nf3_test2" = test_nf3 "nf3/test2.hll"
let%test "nf3_test3" = test_nf3 "nf3/test3.hll"
let%test "nf3_test4" = test_nf3 "nf3/test4.hll"

(******************************************************************************)
(*                                       NF4                                  *)
(******************************************************************************)

let test_nf4 fname = match read_file fname with
  "" -> false
| s -> s |> parse |> nf0 |> nf1 |> nf2 |> nf3 |> nf4 |> is_nf4

let%test "nf4_test0" = test_nf4 "nf4/test0.hll"
let%test "nf4_test1" = test_nf4 "nf4/test1.hll"
let%test "nf4_test2" = test_nf4 "nf4/test2.hll"
let%test "nf4_test3" = test_nf4 "nf4/test3.hll"

(******************************************************************************)
(*                                    Typecheck                               *)
(******************************************************************************)

open IllumLib.Typecheck

let test_typecheck b fname = match read_file fname with
  "" -> false
| s -> s |> parse |> nf0 |> fun c -> b = ok_typecheck c

let%test "typecheck_test0" = test_typecheck false "typecheck/test0.hll"
let%test "typecheck_test1" = test_typecheck false "typecheck/test1.hll"
let%test "typecheck_test2" = test_typecheck false "typecheck/test2.hll"
let%test "typecheck_test3" = test_typecheck false "typecheck/test3.hll"
let%test "typecheck_test4" = test_typecheck false "typecheck/test4.hll"
let%test "typecheck_test5" = test_typecheck false "typecheck/test5.hll"
let%test "typecheck_test6" = test_typecheck false "typecheck/test6.hll"
let%test "typecheck_test7" = test_typecheck false "typecheck/test7.hll"
let%test "typecheck_test8" = test_typecheck false "typecheck/test8.hll"
let%test "typecheck_test9" = test_typecheck false "typecheck/test9.hll"
let%test "typecheck_test10" = test_typecheck false "typecheck/test10.hll"
let%test "typecheck_test11" = test_typecheck false "typecheck/test11.hll"
(* let%test "typecheck_test12" = test_typecheck false "typecheck/test12.hll" *) (* FIXME: view issue are detected in NF0 *)
let%test "typecheck_test13" = test_typecheck false "typecheck/test13.hll"
let%test "typecheck_test14" = test_typecheck false "typecheck/test14.hll"
let%test "typecheck_test15" = test_typecheck false "typecheck/test15.hll"


(******************************************************************************)
(*                                       HLLC                                 *)
(******************************************************************************)

let test_hllc fname = match read_file fname with
  "" -> false
| s -> s |> parse |> hllc |> fun _ -> true

let%test "hllc_test0" = test_hllc "hllc/test0.hll"
let%test "hllc_test1" = test_hllc "hllc/test1.hll"
let%test "hllc_test2" = test_hllc "hllc/test2.hll"
let%test "hllc_test3" = test_hllc "hllc/test3.hll"
let%test "hllc_test4" = test_hllc "hllc/test4.hll"
let%test "hllc_test5" = test_hllc "hllc/test5.hll"
let%test "hllc_test6" = test_hllc "hllc/test6.hll"
let%test "hllc_test7" = test_hllc "hllc/test7.hll"
