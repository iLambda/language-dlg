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

(* convert a reason for error *)
let string_of_pcode_error (error:pcode_error) =
  (* get the reason *)
  let reason = match error.reason with
    | PcodeErrorSpecialTypeLiteralFound -> "A special literal type (void, 't) has no opcode."
  (* construct the message *)
  in ("P-code error : " ^ reason)

let string_of_compile_error (error:compile_error) =
  (* get the reason *)
  let reason = match error.reason with
    | CompileErrorGotoUnbound -> "A goto with no label associated was found."
  (* construct the message *)
  in ("Compilation error : " ^ reason)
