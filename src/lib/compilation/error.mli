(* The reason for the error *)
type pcode_error_reason =
  (* Special type literal detected *)
  | PcodeErrorSpecialTypeLiteralFound
(* The reason for the error *)
type compile_error_reason =
  (* No label associated to the goto *)
  | CompileErrorGotoUnbound

(* The error type *)
type pcode_error = {
  reason: pcode_error_reason
}
(* The error type *)
type compile_error = {
  reason: compile_error_reason
}

(* An exception raised whenever there is an error in the p-code generation *)
exception Pcode_error of pcode_error
(* An exception raised whenever there is an error in the compilation *)
exception Compile_error of compile_error

(* convert a reason to error *)
val string_of_pcode_error : pcode_error -> string
(* convert a reason to error *)
val string_of_compile_error : compile_error -> string
