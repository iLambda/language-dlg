open Datastack
open Env
open Progbuf
open Io

type cpu = {
  mutable progbuf: progbuf option;
  stack: datastack;
  environment: env;
}

(* Makes a cpu *)
val cpu_make : unit -> cpu
(* Bind a progbuf *)
val cpu_bind : cpu -> progbuf-> unit
(* start the cpu *)
val cpu_step : cpu -> io -> unit
