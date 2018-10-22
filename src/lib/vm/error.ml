(* The reason for a vm error *)
type vm_error_reason =
  | VmUnboundVariable
  | VmUnexpectedEOP

(* The payload of a vm error *)
type vm_error = {
  reason: vm_error_reason
}

(* Error raised when the vm tries to do something not ok *)
exception Vm_error of vm_error

(* To string *)
let string_of_vm_error error =
  (* Get the reason *)
  let reason = match error.reason with
    | VmUnboundVariable -> "Variable unbound"
    | VmUnexpectedEOP -> "Unexpected end of program"
  in
  (* the message *)
  "Program error : " ^ reason
