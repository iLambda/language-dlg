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
  (* Ask choice *)
  print_string " ?  ";
  (* The response *)
  let chosen = (read_int () - 1) in
  (* newline *)
  print_newline();
  chosen
