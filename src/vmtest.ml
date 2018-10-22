open Printf
open Vm

(* Print usage *)
let usage () = eprintf "usage: [input: dlgp file]\n"; exit 1
(* Main function *)
let main () =
  (* Check argument list, if not ok print usage  *)
  if Array.length Sys.argv <> 2 then usage ();
  (* Open file *)
  let c = open_in Sys.argv.(1) in
  (* Make a new vm *)
  let vm = vm_make () in
  (* print a header*)
  print_string "(*********************************************)\n";
  print_string "(*            DLGP VIRTUAL MACHINE           *)\n";
  print_string "(*********************************************)\n";
  (* Run from channel *)
  vm_run_from_file vm c;
  (* Close opened stream *)
  close_in c

(* Print the uncaught exceptions *)
let _ = Printexc.catch main ()
