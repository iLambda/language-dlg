open Lexing
open DlgParser
open DlgAST
open Printf

let usage () = eprintf "usage: dlg file\n"; exit 1

let unlocate x = Position.value x

let main () =
  if Array.length Sys.argv <> 2 then usage ();
  let c = open_in Sys.argv.(1) in
  let lb = from_channel c in
  let ast = try DlgParser.program (DlgLexer.main false) lb
            with
              |  Parsing.Parse_error -> Error.print_syntax_error_at c lb; exit 0
            in
  let str = string_of_bool (Typing.check_program_type ast) in
  print_string (str ^ "\n");

  close_in c

let _ = Printexc.catch main ()
