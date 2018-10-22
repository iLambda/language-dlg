open Datastack
open Env

(* the virtual machine *)
type vm = {
  mutable program_counter: int64;
  stack: datastack;
  environment: env;
}

(* create a virtual machine in the default state *)
val vm_make : unit -> vm

(* start the vm with bytecode coming from a file *)
val vm_run_from_file : vm -> in_channel -> unit
