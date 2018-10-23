open DLG.Ast

(* An opcode for the outputted p-code *)
type opcode =
  (* End of data *)
  | OpcEOD
  (* Instructions opcodes *)
  | OpcSet | OpcIfnset
  | OpcMessage of messageopt list
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
  (* A nop instruction *)
  | OpcNop

(* A p-code*)
type pcode

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
val of_opcode : opcode-> bytes

(* Writes to an output channel *)
val to_out : out_channel -> pcode -> unit

(* Concat *)
val concat : pcode list -> pcode

(* Returns the p-code of an expression *)
(* val of_expr : expression -> pcode *)
(* Returns the p-code of a literal *)
(* val of_literal : literal -> pcode *)
(* Returns the p-code of a variable *)
(* val of_var : scoped_identifier -> pcode *)
(*
(* Returns bytes from an opcode *)
val bytes_of_opcode : opcode -> bytes
(* Returns an array of bytes *)
val to_bytes : pcode -> bytes
(* Writes to a file *)
val to_file : string -> pcode -> unit *)
