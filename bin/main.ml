open IllumLib.Utils
open IllumLib.Prettyprint
open IllumLib.Nf
       
(* read file, and output it to a string *)

let read_file filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch; s

(* read line from standard input, and output it to a string *)

(* let read_line () =
  try Some(read_line())
  with End_of_file -> None
;; 
*)

let _ = match Array.length(Sys.argv) with
(* parse / read input from file *) 
| 3 when Sys.argv.(1)="parse" -> (match read_file Sys.argv.(2) with
      "" -> print_newline()
    | s -> s |> parse |> nf0 |> string_of_contractNF |> print_endline)
(* check NF1 / read input from file *) 
| 3 when Sys.argv.(1)="is_nf1" -> (match read_file Sys.argv.(2) with
      "" -> print_newline()
    | s -> s |> parse |> nf0 |> is_nf1 |> string_of_bool |> print_endline)
(* converto to NF1 / read input from file *) 
| 3 when Sys.argv.(1)="nf1" -> (match read_file Sys.argv.(2) with
      "" -> print_newline()
    | s -> s |> parse |> nf0 |> nf1 |> string_of_contractNF |> print_endline)
| 3 when Sys.argv.(1)="nf2" -> (match read_file Sys.argv.(2) with
    "" -> print_newline()
  | s -> s |> parse |> nf0 |> nf1 |> nf2 |> string_of_contractNF |> print_endline)

(* wrong usage *)      
| _ -> failwith "Usage: dune exec illum [parse] [filename]"