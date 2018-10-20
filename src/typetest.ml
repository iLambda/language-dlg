open Lexing
open Printf

(* Print usage *)
let usage () = eprintf "usage: dlg file\n"; exit 1
(* Main function *)
let main () =
  (* Check argument list, if not ok print usage  *)
  if Array.length Sys.argv <> 2 then usage ();
  (* Open file *)
  let c = open_in Sys.argv.(1) in
  (* Create lexbuf *)
  let lb = from_channel c in
  (* Parse *)
  let ast = try DLG.Parser.program (DLG.Lexer.main false) lb
            with
              |  Parsing.Parse_error -> print_string "ah"; exit 0
            in
  (* Check typing *)
  let () = try Typing.Check.check_program_type ast
               with
                | Typing.Error.Type_error e -> Typing.Error.print_type_error_at e (Some c); exit 0
               in
  (* Log no type error, and quit *)
  print_string "Type ok. \n";
  close_in c

(* Print the uncaught exceptions *)
let _ = Printexc.catch main ()
