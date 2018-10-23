open Lexing
open Printf

(* Print usage *)
let usage () = eprintf "usage: [input: dlg file] [output: dlgp file]\n"; exit 1
(* Main function *)
let main () =
  (* Check argument list, if not ok print usage  *)
  if Array.length Sys.argv <> 3 then usage ();
  (* Open file *)
  let c = open_in Sys.argv.(1) in
  (* Create lexbuf *)
  let lb = from_channel c in
  (* Parse *)
  let ast = try DLG.Parser.program (DLG.Lexer.main false) lb
            with
              |  DLG.Parser.Error -> Utils.Error.print_syntax_error_at c lb; exit 1
            in
  (* Check typing *)
  let () = try Typing.Checker.check_program_type ast
               with
                | Typing.Error.Type_error e -> Typing.Error.print_type_error_at e (Some c); exit 1
               in
  (* Create the p-code *)
  let pcode = Pcode.of_program ast in
  (* Send into file *)
  let outfile = open_out_gen [Open_wronly; Open_creat; Open_trunc; Open_binary] 777 Sys.argv.(2) in
  Pcode.to_out outfile pcode;
  close_out outfile;
  (* Log no type error, and quit *)
  Pcode.to_out stdout pcode;
  close_in c

(* Print the uncaught exceptions *)
let _ = Printexc.catch main ()
