open Error
open Progbuf
open Cpu
open Io

(* the virtual machine *)
type vm = {
  cpu: cpu;
  io: io;
}

(* Error raised when the vm tries to do something not ok *)
exception Vm_error of vm_error


(* create a virtual machine in the default state *)
val make : unit -> vm
(* start the vm with the given progbuf *)
val run : vm -> progbuf -> unit


(* create progbuf from file *)
val progbuf_from_file : in_channel -> progbuf

(* Returns a string describing the vm error *)
val string_of_error : vm_error -> string
