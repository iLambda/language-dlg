open DLG.Ast
open Stream

(* An opcode for the outputted p-code *)
type opcode =
  | OpcEop (* The end of a program *)

(* A p-code*)
type pcode = opcode list

(* Raised when *)
exception Pcode_end_of_program

(* Returns the p-code of a program *)
val of_program : program -> pcode

(* Returns bytes from an opcode *)
val bytes_of_opcode : opcode -> bytes
(* Returns a stream of bytes *)
val to_byte_stream : pcode -> bytes Stream.t
(* Returns an array of bytes *)
val to_bytes : pcode -> bytes
(* Writes to an output channel *)
val to_out : out_channel -> pcode -> unit
(* Writes to a file *)
val to_file : string -> pcode -> unit
