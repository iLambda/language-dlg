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


(* create a new io channel *)
val io_make : unit -> io
(* Displays a message in the VM I/O*)
val io_send_message : io -> io_msg_options list -> string -> unit
(* Set the speed *)
val io_set_speed : io -> float -> unit
(* A choice *)
val io_ask_choice : io -> string list -> int
