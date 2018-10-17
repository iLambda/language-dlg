open Lexing
open DlgParser
open DlgAST
open Printf
open Typing

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
  let () = try ignore(Typing.check_program_type ast)
               with
                | Typing.Type_error te -> Typing.print_type_error_at c te; exit 0
               in
  print_string "Type ok. \n";
  close_in c

let _ = Printexc.catch main ()
