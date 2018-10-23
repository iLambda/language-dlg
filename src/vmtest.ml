open Printf

(* Print usage *)
let usage () = eprintf "usage: [input: dlgp file]\n"; exit 1
(* Main function *)
let main () =
  (* Check argument list, if not ok print usage  *)
  if Array.length Sys.argv <> 2 then usage ();
  (* Open file *)
  let c = open_in Sys.argv.(1) in
  (* Make progbuf *)
  let pb = Vm.progbuf_from_file c in
  (* Make a new vm *)
  let vm = Vm.make () in
  (* print a header*)
  print_string "(*********************************************)\n";
  print_string "(*            DLGP VIRTUAL MACHINE           *)\n";
  print_string "(*********************************************)\n";
  (* Run from channel *)
  try Vm.run vm pb with
    | Vm.Vm_error e -> print_string (Vm.string_of_error e);
  (* Close opened stream *)
  close_in c

(* Print the uncaught exceptions *)
let _ = Printexc.catch main ()
