open DLG.Ast
open Error
open Utils.Position
open Utils.Buf

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
type pcode = Buffer.t

(* Concat *)
let concat pcodes = match pcodes with
  (* Error *)
  | [] -> raise (Invalid_argument "Can't concat an empty p-code list")
  (* Else, take first, accumulate and return *)
  | buf::tail -> List.iter (Buffer.add_buffer buf) tail; buf

(* Returns the p-code of a program *)
let rec of_program program =
  (* the current program insctruction and the rest of the program *)
  let program_instr = ref INop in
  let program_left = ref program in
  (* create the program buffer  *)
  let program_buffer = Buffer.create 128 in
  (* while there are instructions *)
  while !program_left <> [] do
    (* get the current instruction *)
    begin match !program_left with
      (* Can't reach *)
      | [] -> assert false
      (* Check instruction *)
      | i::rest ->
        (* get current instruction *)
        program_instr := (value i);
        (* get rest of program *)
        program_left := rest;
        (* add the current instruction p-code *)
        Buffer.add_buffer program_buffer (of_instruction !program_instr)
    end;
  done;
  (* return the buffer *)
  program_buffer


(* Returns the p_code of an expression*)
and of_expression expr = match expr with
  (* A variable *)
  | EVar scopedid -> of_variable (value scopedid)
  (* A literal *)
  | ELiteral literal ->
    (* get the type *)
    let t = Typing.Type.type_of_literal (value literal) in
    (* Check the literal type *)
    begin match (value literal) with
      (* vectors push expressions first and constructor later *)
      | LVec2 (x,y) -> concat [
          of_expression (value y);
          of_expression (value x);
          (* the literal type *)
          from_bytes (of_opcode (OpcLiteral t));
        ]
      | LVec3 (x,y,z) -> concat [
          of_expression (value z);
          of_expression (value y);
          of_expression (value x);
          (* the literal type *)
          from_bytes (of_opcode (OpcLiteral t));
        ]
      (* Any other literal  pushes constructor first and data later*)
      | _ ->
          concat [
              (* the literal type *)
              from_bytes (of_opcode (OpcLiteral t));
              (* match the literal and send the data*)
              match (value literal) with
                | LInt i -> Utils.Buf.int32_to_buf i
                | LFloat f -> Utils.Buf.float_to_buf f
                | LBool b -> Utils.Buf.bool_to_buf b
                | LString fstr -> of_fstring fstr;
                | LEnum _ -> failwith "Enum literal not done yet"
                | _ -> assert false
            ]
    end
  (* An operation *)
  | EOperation (op, lhs, rhs) -> concat [
      (* push operands *)
      of_expression (value lhs);
      of_expression (value rhs);
      (* the literal type *)
      from_bytes (of_opcode (OpcOperation (value op)));
    ]

  (* A condition *)
  | ECondition (cond, thendo, elsedo) -> concat [
      (* push operands *)
      of_expression (value elsedo);
      of_expression (value thendo);
      of_expression (value cond);
      (* the literal type *)
      from_bytes (of_opcode OpcTernary);
    ]

  (* A function call *)
  | EFunc (identifier, arglist) -> concat
    (* all the expressions *)
    ((List.rev_map of_expression (List.map value (value arglist))) @ [
      (* the identifier *)
      of_scoped_identifier (SExtern, (value identifier));
      (* the opcode *)
      from_bytes (of_opcode OpcFunction);
    ])

  (* An access to a constructed type *)
  | _ -> failwith "Unimplemented access & cast yet"

(* Returns the p-code of an instruction *)
and of_instruction = function
  (* A nop instruction *)
  | INop -> from_bytes (of_opcode OpcNop)
  (* A variable set *)
  | ISet (scopedid, expr) -> concat [
      (* the expression *)
      of_expression (value expr);
      (* the scoped identifier*)
      of_scoped_identifier (value scopedid);
      (* the opcode*)
      from_bytes (of_opcode OpcSet);
    ]
  (* A variable ifnet *)
  | IIfnset (scopedid, expr) -> concat [
      (* the expression *)
      of_expression (value expr);
      (* the scoped identifier*)
      of_scoped_identifier (value scopedid);
      (* the opcode*)
      from_bytes (of_opcode OpcIfnset);
    ]
  (* A message *)
  | IMessage (msg) ->
    (* destruct *)
    let fstr, opts = (value msg) in
    (* Return the buffer *)
    concat [
        (* the fstring *)
        of_fstring fstr;
        (* the opcode *)
        from_bytes (of_opcode (OpcMessage (List.rev_map value opts)));
      ]

  | _ -> assert false

(* Returns the p-code of an opcode *)
and of_opcode opcode =
  (* get the byte string *)
  let bytestring = match opcode with
    (* Instructions *)
    | OpcSet -> [ 0x20 ]
    | OpcIfnset -> [ 0x21 ]
    | OpcMessage opts ->
      (* the function returning the flag value for a given option*)
      let message_opt_flag opt = match opt with
        | MsgNoRush -> 0x01
        | MsgNoAcknowledge -> 0x02
      (* Turn the list into a flag *)
      in 0x22::[(List.fold_left (+) 0 (List.rev_map message_opt_flag opts))]

    (* A variable *)
    | OpcIdentifier scope -> begin match scope with
        | SExtern -> [ 0x60 ]
        | SGlobal -> [ 0x61 ]
        | SLocal -> [ 0x62 ]
      end
    (* A nop *)
    | OpcNop -> [ 0x80 ]
    (* A variable *)
    | OpcVariable scope -> begin match scope with
        | SExtern -> [ 0x81 ]
        | SGlobal -> [ 0x82 ]
        | SLocal -> [ 0x83 ]
      end
    (* A literal *)
    | OpcLiteral t -> begin match t with
      | TInt -> [ 0x90 ]
      | TFloat -> [ 0x91 ]
      | TBool -> [ 0x92 ]
      | TString -> [ 0x93 ]
      | TEnum _ -> [ 0x94 ] (* TODO : fix *)
      | TVec2 -> [ 0x95 ]
      | TVec3 -> [ 0x96 ]
      | _ -> raise (Pcode_error { reason=PcodeErrorSpecialTypeLiteralFound })
    end
    (* Inline expr and string consants *)
    | OpcInline -> [ 0x9F ]
    (* An operation *)
    | OpcOperation o -> 0xA0::begin match o with
      | OpPlus -> [ 0x00 ]
      | OpMinus -> [ 0x01 ]
      | _ -> assert false
    end
    (* A ternary token*)
    | OpcTernary -> [ 0xA2 ]
    (* A function call *)
    | OpcFunction -> [ 0xA3 ]
    (* A type cast *)
    | OpcCast t -> 0xA4:: 
    (* End of data *)
    | OpcEOD -> [ 0x00 ]
    (* generate the bytes *)
    in byte_from_list bytestring

(* Returns the p-code of a scoped identifier*)
and of_scoped_identifier scopedid =
  (* destruct *)
  let scope, id = scopedid in
  (* Make default buffer *)
  let buf = from_bytes (of_opcode (OpcIdentifier scope)) in
  (* Add the string and terminate *)
  write_identifier buf id;
  (* Return the buffer *)
  buf

(* Returns the p-code of a fstring *)
and of_fstring fstr =
  (* outputs the pcode for a token *)
  let of_fstring_tok = function
    (* A constant string *)
    | StrConst s ->
      (* make the buffer *)
      let buffer = Utils.Buf.str_to_buf s in
      (* Append a EOD *)
      Buffer.add_bytes buffer (of_opcode OpcEOD);
      (* Return *)
      concat [
          (* the value type *)
          from_bytes (of_opcode (OpcLiteral TString));
          (* the contents *)
          buffer
      ]
    (* An inline expr *)
    | StrInline e ->
      (* return the buffer for the expression and add a inline marker *)
      concat [
        of_expression (value e);
        from_bytes (of_opcode OpcInline)
      ]
    (*  *)
    | StrColor _ -> assert false
  in
  (* glue the tokens together *)
  let rec fstring_glue fstr = match fstr with
    (* Empty fstring *)
    | [] -> concat [
        (* the value type *)
        from_bytes (of_opcode (OpcLiteral TString));
        (* the contents *)
        from_bytes (of_opcode (OpcEOD))
    ]
    (* Single token : good on its own *)
    | [t] -> of_fstring_tok (value t)
    (* Two tokens : put then glue *)
    | [t1; t2] -> concat [
        (* the tokens *)
        of_fstring_tok (value t2);
        of_fstring_tok (value t1);
        (* the glue *)
        from_bytes (of_opcode (OpcOperation OpPlus));
    ]
    (* More : glue and recursively call*)
    | t::tail -> concat [

      fstring_glue tail;
      of_fstring_tok (value t);
      (* the glue *)
      from_bytes (of_opcode (OpcOperation OpPlus));
    ]
  in
  (* concat the pcode for each token *)
  fstring_glue fstr

(* Returns the p-code of an expression variable *)
and of_variable scopedid =
  (* destruct *)
  let scope, id = scopedid in
  (* Make default buffer *)
  let buf = from_bytes (of_opcode (OpcVariable scope)) in
  (* Add the string and terminate *)
  write_identifier buf id;
  (* Return the buffer *)
  buf

(* Write identifier in the buffer *)
and write_identifier buf id = match id with
  | Id s -> Buffer.add_string buf s;
  (* Terminate *)
  Buffer.add_bytes buf (of_opcode OpcEOD)


(* Writes to an output channel *)
let to_out channel pcode =
  (* Write *)
  Buffer.output_buffer channel pcode
