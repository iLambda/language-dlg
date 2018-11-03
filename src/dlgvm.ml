(* the filenames *)
let sourcefilename = ref (None:string option)
(* options *)
let opt_print = ref false

let usage = "usage: " ^ Sys.argv.(0) ^ " script.dlp [options]"
let speclist = [
    ("--stdout", Arg.Set opt_print, ": print the bytecode to stdout")
  ]
let filenames x =
  if !sourcefilename = None then sourcefilename := Some x

let () =
  (* Read the arguments *)
  Arg.parse
    speclist
    filenames
    usage;
  (* Try get the source filename *)
  match !sourcefilename with
    (* None, error *)
    | None -> failwith "No filename specified"
    (* There is one *)
    | Some source ->
      (* Open file *)
      let c = open_in source in
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
