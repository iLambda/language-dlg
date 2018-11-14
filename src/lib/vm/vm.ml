open Progbuf
open Cpu
open Io
open Lwt
open Error

(* Export modules *)
module Error = Error
module Progbuf = Progbuf

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
  let rec loop () =
    (* check if eof *)
    if progbuf.eof () then return ()
    (* not EOF *)
    else
      (* run one step *)
      cpu_step vm.cpu vm.io
      (* recur *)
      >>= fun () -> loop ()
  in
  let main () =
    (* start io *)
    io_enable vm.io
    (* bind progbuf *)
    >>= fun mode -> return (cpu_bind vm.cpu progbuf)
    (* init random *)
    >>= fun () -> Random.self_init (); return ()
    (* start *)
    >>= fun () -> Lwt.finalize
          (* do loop *)
          (fun () -> (Lwt.catch
                      (fun () -> loop ())
                      (function
                        (* An IO Break *)
                        | Io_break -> return ()
                        (* A wrong data/variable type is carried toa vm error *)
                        | Data.Wrong_data_type { expected; token } ->
                          raise (Vm_error { reason= (VmWrongDataType (expected, token)) })
                        | Data.Wrong_variable_type { expected; token } ->
                          raise (Vm_error { reason= (VmWrongVariableType (expected, token)) })
                        (* Raise any other exn *)
                        | exn -> raise exn)))
          (* at end, disable *)
          (fun () -> io_disable vm.io mode)
  (* run *)
  in Lwt_main.run (main ())
