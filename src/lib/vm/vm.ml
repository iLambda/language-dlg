open Error
open Progbuf
open Cpu
open Io
open Lwt
(* open LTerm_widget *)
(* open LTerm_event *)
(* open CamomileLibrary *)

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
    (* start *)
    >>= fun () -> Lwt.finalize
          (* do loop *)
          (fun () -> (Lwt.catch
                      (fun () -> loop ())
                      (function
                        (* An IO Break *)
                        | Io_break -> return ()
                        (* Raise any other exn *)
                        | exn -> raise exn)))
          (* at end, disable *)
          (fun () -> io_disable vm.io mode)
  (* run *)
  in Lwt_main.run (main ())

(* create progbuf from file *)
let progbuf_from_file file = Progbuf.progbuf_from_file file

(* To string *)
let string_of_error error = Error.string_of_vm_error error
