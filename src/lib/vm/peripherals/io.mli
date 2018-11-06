open LTerm

(* An input *)
type io_input =
  (* Rush a message / acknowledge *)
  | Enter
  (* Quit VM *)
  | Quit
  (* Directional keys *)
  | Up | Down | Left | Right

(* IO message options *)
type io_msg_option =
  | MsgNoRush
  | MsgNoAcknowledge
type io_msg_options = io_msg_option list

(* A message *)
type io_message = string * io_msg_options

(* represents an IO channel *)
type io = {
  (* the base speed & speed modifier *)
  basespeed: float;
  mutable speed: float;
  (* a message mailbox *)
  (* message: io_message Lwt_mvar.t; *)
  (* an input key mailbox *)
  input: io_input Lwt_mvar.t;
  (* the terminal and its old mode *)
  terminal: LTerm.t;
}

(* A break exception *)
exception Io_break 

(* create a new io channel *)
val io_make : unit -> io
(* Enable *)
val io_enable : io -> mode Lwt.t
(* Disable and return the terminal to mode *)
val io_disable : io -> mode -> unit Lwt.t

(* Displays a message in the VM I/O*)
val io_send_message : io -> io_msg_options -> string -> unit Lwt.t
(* Set the speed *)
val io_set_speed : io -> float -> unit
(* A choice *)
val io_ask_choice : io -> string list -> int Lwt.t
