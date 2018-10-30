(* The reason for the error *)
type pcode_error_reason =
  (* Special type literal detected *)
  | PcodeErrorSpecialTypeLiteralFound
(* The error type *)
type pcode_error = {
  reason: pcode_error_reason
}

(* An exception raised whenever there is an error in the p-code generation *)
exception Pcode_error of pcode_error

(* convert a reason for error *)
val string_of_pcode_error : pcode_error -> string
