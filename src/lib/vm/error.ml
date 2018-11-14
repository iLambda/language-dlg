open Data

(* The reason for a vm error *)
type vm_error_reason =
  | VmUnboundVariable
  | VmUnexpectedEOP
  | VmUnrecognizedDeclarator
  | VmExternAccessUnsupported
  | VmIllFormedProgram
  | VmMemEmpty
  | VmUnrecognizedOperation
  | VmAlreadyBoundVariable
  | VmWrongDataType of string * data
  | VmWrongVariableType of value * value

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
    | VmUnrecognizedDeclarator -> "Declarator not recognized"
    | VmExternAccessUnsupported -> "Extern access in this VM is not allowed"
    | VmIllFormedProgram -> "The p-code is ill-formed"
    | VmMemEmpty -> "Memory was empty"
    | VmUnrecognizedOperation -> "Operation not recognized"
    | VmAlreadyBoundVariable -> "Variable was already bound"
    | VmWrongDataType (_expected, _given) -> "Data type mismatch : expected ?, given ?"
    | VmWrongVariableType (_expected, _given) -> "Variable type mismatch : expected ?, given ?"
  in
  (* the message *)
  "Program error : " ^ reason ^ "\n"
