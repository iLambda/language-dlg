open Lexing

(* the filenames *)
let sourcefilename = ref (None:string option)
let destfilename = ref (None:string option)
(* options *)
let opt_print = ref false

let usage = "usage: " ^ Sys.argv.(0) ^ " input.dlg output.dlp [options]"
let speclist = [
    ("--stdout", Arg.Set opt_print, ": print the bytecode to stdout")
  ]
let filenames x =
  if !sourcefilename = None then sourcefilename := Some x
  else if !destfilename = None then destfilename := Some x

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
      (* Create lexbuf *)
      let lb = from_channel c in
      (* Parse *)
      let ast = try DLG.Parser.program (DLG.Lexer.main false) lb
                with
                  |  DLG.Parser.Error -> Utils.Error.print_syntax_error_at c lb;
                                         exit 1
                in
      (* Check typing *)
      let () = try Typing.Checker.check_program_type ast
                   with
                    | Typing.Error.Type_error e -> Typing.Error.print_type_error_at e (Some c);
                                                   exit 1
                   in
      (* Optimize ast *)
      let optiast = Optimiser.optimise_program ast in
      (* Create the p-code *)
      let pcode = Compilation.Compiler.compile optiast in
      (* Close input file *)
      close_in c;

      (* Check if print to stdout *)
      if !opt_print
      then Compilation.Pcode.to_out stdout pcode;
      (* Check if out destination *)
      match !destfilename with
        (* None; do nothing *)
        | None -> ()
        (* There is one, print *)
        | Some dest ->
          (* Send into file *)
          let outfile = open_out_gen [Open_wronly; Open_creat; Open_trunc; Open_binary] 777 dest in
          Compilation.Pcode.to_out outfile pcode;
          close_out outfile
