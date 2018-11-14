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
      let pb = Vm.Progbuf.progbuf_from_file c in
      (* Make a new vm *)
      let vm = Vm.make () in

      (* Run from channel *)
      try Vm.run vm pb with
        (* a VM error *)
        | Vm.Error.Vm_error e -> print_string (Vm.Error.string_of_vm_error e);
      (* Close opened stream *)
      close_in c
