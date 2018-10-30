open DLG.Ast
open Error
open Utils

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
  (* Jump tables *)
  | OpcJump of int32 * int64
  | OpcJumpTableEnd
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
  | OpcGotoId of identifier
  | OpcLabelId of identifier
  | OpcGoto of int32
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

type t = opcode Dlist.t
type pcode = t

(* Concatenate some p-code *)
let concat pcodes = Dlist.concat pcodes
(* Empty p-code *)
let empty () = Dlist.empty ()
(* Create from list *)
let of_list l = Dlist.of_list l
(* Append two pcodes *)
let append a b = Dlist.append a b
(* Maps the pcode *)
let mapi pcode f = Dlist.mapi pcode ~f:f

(* Returns the byte sequence containing the identifier*)
let identifier_to_buffer buf = function
  | Id s ->
    (* append *)
    Buffer.add_string buf s;
    Buffer.add_char buf '\x00'

(* Return the length *)
let length pcode = Int64.of_int (Dlist.length pcode)
(* Return the nth instruction *)
let nth (pcode:pcode) n = Dlist.nth_exn pcode (Int64.to_int n)
(* Iterate the pcode *)
let iter (pcode:pcode) f = Dlist.iter pcode ~f:f

(* Return the byte sequence for a given opcode *)
let rec byte_sequence_of_opcode = function
  (* end of data *)
  | OpcEOD -> Buf.byte_from_list [0x00]
  (*
   * FLOW CONTROL & STACK/ENV MANAGEMENT
   *)
   (* Stack management *)
  | OpcMem -> Buf.byte_from_list [0x01]
  | OpcDupl -> Buf.byte_from_list [0x02]
  | OpcDeepenScope -> Buf.byte_from_list [0x03]
  | OpcRaiseScope -> Buf.byte_from_list [0x04]
   (* Control flow *)
  | OpcSkipIfNot s -> Buf.byte_from_list (0x10::(Buf.byte_list_of_int64 s))
  | OpcSkip s -> Buf.byte_from_list (0x11::(Buf.byte_list_of_int64 s))
  (*
   * INSTRUCTIONS
   *)
  | OpcSet -> Buf.byte_from_list [0x20]
  | OpcIfnset -> Buf.byte_from_list [0x21]
  | OpcInit -> Buf.byte_from_list [0x22]
  | OpcMessage opts ->
    (* the function returning the flag value for a given option*)
    let message_opt_flag opt = match opt with
      | MsgNoRush -> 0x01
      | MsgNoAcknowledge -> 0x02 in
    (* Turn the list into a flag *)
    let flag = (List.fold_left (+) 0 (List.rev_map message_opt_flag opts)) in
    (* Return the command *)
    Buf.byte_from_list [0x23; flag]
  | OpcWait -> Buf.byte_from_list [ 0x24 ]
  | OpcSpeed -> Buf.byte_from_list [ 0x25 ]
  | OpcInvoke -> Buf.byte_from_list [ 0x26 ]
  | OpcSend -> Buf.byte_from_list [ 0x27 ]
  | OpcChoice -> Buf.byte_from_list [ 0x28 ]
  | OpcGotoId _ -> byte_sequence_of_opcode (OpcGoto (-1l))
  | OpcGoto l ->
    (* convert int to buffer *)
    let buffer = Utils.Buf.int32_to_buf l in
    (* return *)
    Bytes.cat (Buf.byte_from_list [ 0x29 ]) (Buffer.to_bytes buffer)
  | OpcLabelId _ -> byte_sequence_of_opcode (OpcNop)
  (* A nop *)
  | OpcNop -> Buf.byte_from_list [0x80]

  (*
   * IDENTIFIERS
   *)
  | OpcIdentifier sid ->
    (* deconstruct identifier *)
    let scope, id = sid in
    (* create buffer *)
    let buffer = Buffer.create 32 in
    (* write scope *)
    let marker = match scope with
      | SExtern -> 0x60
      | SLocal -> 0x61
      | SGlobal -> 0x62
    in Buffer.add_char buffer (Char.chr marker);
    (* write identifier as null terminated string *)
    identifier_to_buffer buffer id;
    (* return *)
    Buffer.to_bytes buffer

  (*
   * EXPRESSIONS
   *)
  | OpcVariable -> Buf.byte_from_list [ 0x8F ]
  | OpcValue v -> begin match v with
    (* an integer *)
    | VInt i  ->
      (* convert int to buffer *)
      let buffer = Utils.Buf.int32_to_buf i in
      (* return *)
      Bytes.cat (Buf.byte_from_list [ 0x90 ]) (Buffer.to_bytes buffer)
    (* a float *)
    | VFloat f ->
      (* convert float to buffer *)
      let buffer = Utils.Buf.float_to_buf f in
      (* return *)
      Bytes.cat (Buf.byte_from_list [ 0x91 ]) (Buffer.to_bytes buffer)
    (* a boolean *)
    | VBool b ->
      (* convert float to buffer *)
      let buffer = Utils.Buf.bool_to_buf b in
      (* return *)
      Bytes.cat (Buf.byte_from_list [ 0x92 ]) (Buffer.to_bytes buffer)
    (* a string *)
    | VString s ->
      (* create buffer *)
      let buffer = Buffer.create (1 + String.length s) in
      (* write header *)
      Buffer.add_char buffer (Char.chr 0x93);
      (* write identifier as null terminated string *)
      Buffer.add_string buffer s;
      Buffer.add_char buffer (Char.chr 0x00);
      (* return *)
      Buffer.to_bytes buffer
    (* an enum *)
    | VEnum _ -> Buf.byte_from_list [ 0x94 ] (*TODO: fix*)
    (* a 2D vector *)
    | VVec2 -> Buf.byte_from_list [ 0x95 ]
    (* a 3D vector *)
    | VVec3 -> Buf.byte_from_list [ 0x96 ]
  end
  | OpcInline -> Buf.byte_from_list [ 0x9F ]
  | OpcOperation o ->
    (* compute operation code *)
    let operationcode = match o with
      | OpPlus -> 0x00
      | OpMinus -> 0x01
      | OpStar -> 0x02
      | OpDivide -> 0x03
      | OpAnd -> 0x04
      | OpOr -> 0x05
      | OpEqual -> 0x06
      | OpNotEqual -> 0x07
      | OpLeq -> 0x08
      | OpGeq -> 0x09
      | OpLess -> 0x0A
      | OpMore -> 0x0B
    (* return bytes *)
    in Buf.byte_from_list [0xA0; operationcode]
  | OpcTernary -> Buf.byte_from_list [ 0xA1 ]
  | OpcFunctionCall -> Buf.byte_from_list [ 0xA2 ]
  | OpcCast t -> Buf.byte_from_list (0xA3::begin match t with
      | TInt -> [ 0x90 ]
      | TFloat -> [ 0x91 ]
      | TBool -> [ 0x92 ]
      | TString -> [ 0x93 ]
      | TEnum _ -> [ 0x94 ] (* TODO : fix *)
      | TVec2 -> [ 0x95 ]
      | TVec3 -> [ 0x96 ]
      | _ -> raise (Pcode_error { reason=PcodeErrorSpecialTypeLiteralFound })
    end)
  | OpcAccess -> Buf.byte_from_list [ 0xA4 ]
  (*
   *  JUMP TABLES
   *)
   | OpcJump (depth, pos) ->
     let depthbuf = Utils.Buf.int32_to_buf depth in
     let posbuf = Utils.Buf.int64_to_buf pos in
     (* concat *)
     Buffer.add_buffer depthbuf posbuf;
     (* return *)
     Buffer.to_bytes depthbuf
   | OpcJumpTableEnd -> Buf.byte_from_list [0xFF]

(* Return the byte length of pcode *)
let byte_length_of pcode =
  (* compute the length of a byte sequence *)
  let bytelen acc opc =
    (* compute the size of opcode *)
    let size = Bytes.length (byte_sequence_of_opcode opc) in
    (* add *)
    Int64.add acc (Int64.of_int size)
  in
  (* fold *)
  Dlist.fold pcode ~init:0L ~f:bytelen

(* Write to channel *)
let to_out out pcode =
  (* write byte function *)
  let write opc =
    (* convert to bytes *)
    let bytes = byte_sequence_of_opcode opc in
    (* write *)
    output_bytes out bytes
  in
  (* write them all *)
  Dlist.iter pcode ~f:write
