open Lexing
open Parser
open Printf

let usage () = eprintf "usage: cpp file\n"; exit 1

let print_tok = function
  | EOF -> "EOF"
  | INDENT -> "INDENT"
  | OUTDENT -> "OUTDENT"
  | _ -> "THING"

let main () =
  if Array.length Sys.argv <> 2 then usage ();
  let c = open_in Sys.argv.(1) in
  let lb = from_channel c in
  let rec tokenize () =
    let tok = Lexer.main false lb in
    print_string (print_tok tok); print_string "\n";
    if tok <> EOF then tokenize () else ()
  in tokenize ();
  close_in c

let _ = Printexc.catch main ()
