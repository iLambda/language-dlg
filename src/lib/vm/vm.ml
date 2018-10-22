open Datastack
open Env

(* the virtual machine *)
type vm = {
  mutable program_counter: int64;
  stack: data_stack;
  environment: env;
}

(* create a virtual machine in the default state *)
let make () = {
  program_counter = 0L;
  stack = Datastack.make ();
  environment = Env.make ();
}

(* start the vm with bytecode coming from a file *)
let run_from_file vm file =
  (* Reset position *)
  seek_in file 0;
  (* Reset program counter *)
  vm.program_counter <- 0L;
  (* Reset local environment *)
  env_clear vm.environment
