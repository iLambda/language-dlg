open Error
open Progbuf
open Cpu
open Io

(* Error raised when the vm tries to do something not ok *)
exception Vm_error of vm_error

(* the virtual machine *)
type vm = {
  cpu: cpu;
  io: io;
}

(* create a virtual machine in the default state *)
let make () = {
  cpu = cpu_make ();
  io = io_make ();
}

(* start the vm with bytecode coming from a file *)
let run vm progbuf =
  (* bind the progbuf to the cpu *)
  cpu_bind vm.cpu progbuf;
  (* run cpu *)
  while not (progbuf.eof ()) do
    (* run for one step *)
    cpu_step vm.cpu vm.io
  done

(* create progbuf from file *)
let progbuf_from_file file = Progbuf.progbuf_from_file file

(* To string *)
let string_of_error error = Error.string_of_vm_error error
