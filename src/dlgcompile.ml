open Lexing

(* the filenames *)
let sourcefilename = ref (None:string option)
let destfilename = ref (None:string option)
(* options *)
let opt_print = ref false
let opt_use_optimizer = ref true
let opt_use_typer = ref true

let usage = "usage: " ^ Sys.argv.(0) ^ " input.dlg output.dlp [options]"
let speclist = [
    (* output *)
    ("--stdout", Arg.Set opt_print, ": print the bytecode to stdout");
    (* disable stages of compilation *)
    ("--no-typecheck", Arg.Clear opt_use_typer, ": do not use the static typer");
    ("--no-optimise", Arg.Clear opt_use_optimizer, ": do not use the optimiser");
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
  (* Disable optimizer if typecheck off *)
  if not !opt_use_typer then opt_use_optimizer := false;
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
      if not !opt_print then print_string "Parsing OK\n";
      (* Check typing *)
      if !opt_use_typer then
        begin
          let () = try Typing.Checker.check_program_type ast
                       with
                        | Typing.Error.Type_error e -> Typing.Error.print_type_error_at e (Some c);
                                                       exit 1
                       in
           if not !opt_print then print_string "Typing OK\n";
        end
      else if not !opt_print then print_string "No typing.\n";
      (* Optimize ast *)
      let optiast = if !opt_use_optimizer then Optimiser.optimise_program ast else ast in
      if not !opt_print then
        begin
          if !opt_use_optimizer
          then print_string "Optimisation OK\n"
          else print_string "No optimisation.\n"
        end;
      (* Create the p-code *)
      let pcode = Compilation.Compiler.compile optiast in
      if not !opt_print then print_string "Compilation OK\n";
      (* Close input file *)
      close_in c;

      (* Compute size *)
      let size = Compilation.Pcode.byte_length_of pcode in
      (* To human readable size *)
      let to_human_readable size =
        (* units list *)
        let units = ["B"; "kB"; "MB"; "GB"; "TB"; "PB"; "EB"; "ZB"; "YB"] in
        (* thresh *)
        let thresh = 1024L in
        (* rec *)
        let rec aux size rem u =
          (* check size *)
          if size < thresh then ((Int64.to_string size) ^ "." ^ (Int64.to_string rem) ^ (List.nth units u))
          (* divide *)
          else
            (* the integer part of the size *)
            let intpart = (Int64.div size thresh) in
            let decpart = 1000. *. ((Int64.to_float (Int64.rem size thresh)) /. (Int64.to_float thresh)) in
            aux intpart (Int64.of_float decpart) (u+1)
        in aux size 0L 0
      in
      (* Print size *)
      if not !opt_print
      then Printf.printf "Bytecode size : %s (%Ld bytes)\n" (to_human_readable size) size;

      (* Check if print to stdout *)
      if !opt_print
      then Compilation.Pcode.to_out stdout pcode
      else print_string "Done !\n";
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
