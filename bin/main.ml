open Illum.Prettyprint
open Illum.Main
       
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
    | s -> s |> parse |> string_of_contract |> print_endline)
(* wrong usage *)      
| _ -> failwith "Usage: dune exec illum [parse] [filename]"