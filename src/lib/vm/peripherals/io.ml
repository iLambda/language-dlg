(* represents an IO channel *)
type io = {
  (* the base speed *)
  basespeed: float;
  (* the speed modifier *)
  mutable speed: float;
}
(* IO message options *)
type io_msg_options =
  | MsgNoRush
  | MsgNoAcknowledge


(* Get one character from the console  *)
let io_get_1_char () =
    let termio = Unix.tcgetattr Unix.stdin in
    let () =
        Unix.tcsetattr Unix.stdin Unix.TCSADRAIN
            { termio with Unix.c_icanon = false } in
    let res = input_char stdin in
    Unix.tcsetattr Unix.stdin Unix.TCSADRAIN termio;
    res

(* Get an int until it is right *)
let rec io_get_int mini maxi =
  print_string " ?  ";
  (* print *)
  let possible_int = read_line () in
  (* try match *)
  match int_of_string possible_int with
    | exception Failure _ ->
      (* go back on track *)
      io_get_int mini maxi
    | v when v >= mini && v < maxi -> v
    | _ ->
      (* go back on track *)
      io_get_int mini maxi

(* Write text char per char *)
let io_write io str =
  (* read each char *)
  for i = 0 to (String.length str) - 1
  do
    (* print *)
    print_char str.[i];
    flush stdout;
    (* sleep *)
    Unix.sleepf (io.basespeed /. io.speed);
  done

(* create a new io channel *)
let io_make () = {
  basespeed = 0.05;
  speed = 1.;
}

(* Displays a message in the VM I/O*)
let io_send_message io opts message =
  (* read each char *)
  io_write io message;
  (* if acknowledge asked *)
  if not (List.mem MsgNoAcknowledge opts)
  then begin
   (* wait till newline *)
    while (io_get_1_char ()) <> '\n' do print_string "\b \b"; flush stdout done;
    (* print newline *)
    print_string "\n"
  end
  (* print newline *)
  else print_string "\n\n"

(* Set the speed *)
let io_set_speed io speed =
  io.speed <- speed


(* A choice *)
let io_ask_choice io choices =
  (* draw each choice *)
  List.iteri
    (fun i s -> io_write io ("(" ^ (string_of_int (i+1)) ^  ") " ^ s ^ "\n"))
    choices;
  (* The response *)
  let chosen = ((io_get_int 1 (List.length choices))) in
  (* newline *)
  print_newline();
  chosen
