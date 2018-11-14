open Progbuf
open Cpu
open Io

(* Export modules *)
module Error = Error
module Progbuf = Progbuf

(* the virtual machine *)
type vm = {
  cpu: cpu;
  io: io;
}

(* create a virtual machine in the default state *)
val make : unit -> vm
(* start the vm with the given progbuf *)
val run : vm -> progbuf -> unit
