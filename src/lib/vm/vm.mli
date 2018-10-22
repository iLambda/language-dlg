open Datastack
open Env

(* the virtual machine *)
type vm = {
  mutable program_counter: int64;
  stack: data_stack;
  environment: env;
}

(* create a virtual machine in the default state *)
val make : unit -> vm

(* start the vm with bytecode coming from a file *)
val run_from_file : vm -> in_channel -> unit
