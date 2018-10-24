open DLG.Ast

(* A p-code*)
type pcode
(* An opcode for the outputted p-code *)
type opcode =
  (* End of data *)
  | OpcEOD
  (* Control flow *)
  | OpcSkipIfNot of int64
  | OpcSkip of int64
  (* Stack&env management *)
  | OpcMem
  | OpcDupl
  | OpcDeepenScope | OpcRaiseScope
  (* Instructions opcodes *)
  | OpcSet | OpcIfnset | OpcInit
  | OpcWait | OpcSpeed | OpcSend
  | OpcInvoke
  | OpcMessage of messageopt list
  | OpcChoice
  (* Scoped identifier opcode *)
  | OpcIdentifier of scope
  (* Expressions opcodes *)
  | OpcVariable of scope
  | OpcLiteral of type_const
  | OpcOperation of operation
  | OpcInline
  | OpcTernary
  | OpcFunction
  | OpcCast of type_const
  | OpcAccess
  (* A nop instruction *)
  | OpcNop

(* Concat *)
val concat : pcode list -> pcode

(* Returns the p-code of a program *)
val of_program : program -> pcode
(* Returns the p_code of an expression*)
val of_expression : expression -> pcode
(* Returns the p-code of an instruction *)
val of_instruction : instruction -> pcode
(* Returns the p-code of a scoped identifeir *)
val of_scoped_identifier : scoped_identifier -> pcode
(* Returns the p-code of a fstring *)
val of_fstring : fstring -> pcode
(* Returns the bytes making of an opcode *)
val of_opcode : opcode -> pcode

(* Writes to an output channel *)
val to_out : out_channel -> pcode -> unit
