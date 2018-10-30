open DLG.Ast

type value =
  | VInt of int32
  | VFloat of float
  | VBool of bool
  | VVec2
  | VVec3
  | VString of string
  | VEnum of identifier * identifier

type opcode =
  (* End of data *)
  | OpcEOD
  (* Control flow *)
  | OpcSkipIfNot of int64
  | OpcSkip of int64
  (* Stack management *)
  | OpcMem
  | OpcDupl
  | OpcDeepenScope | OpcRaiseScope
  (* Data *)
  | OpcIdentifier of scoped_identifier
  | OpcValue of value
  (* Classic instructions *)
  | OpcSet | OpcIfnset | OpcInit
  | OpcWait
  | OpcMessage of messageopt list
  | OpcInvoke
  | OpcSpeed
  | OpcSend
  | OpcChoice
  | OpcGoto of identifier
  | OpcLabel of identifier
  (* Expressions *)
  | OpcVariable
  | OpcOperation of operation
  | OpcTernary
  | OpcFunctionCall
  | OpcCast of type_const
  | OpcAccess
  | OpcInline
  (* A nop instruction *)
  | OpcNop

type t
type pcode = t

(* Empty p-code *)
val empty : unit -> pcode
(* Concatenate some p-code *)
val concat : pcode list -> pcode
(* Append two pcodes *)
val append : pcode -> pcode -> pcode
(* Create from list *)
val of_list : opcode list -> pcode
(* Return the length *)
val length : pcode -> int64
(* Return the nth instruction *)
val nth : pcode -> int64 -> opcode
(* Maps the pcode *)
val mapi : pcode -> (int -> opcode -> opcode) -> pcode

(* Return the byte length of pcode *)
val byte_length_of : pcode -> int64
(* Return the byte sequence for a given opcode *)
val byte_sequence_of_opcode : opcode -> bytes

(* Write to channel *)
val to_out : out_channel -> pcode -> unit
