open DLG.Ast
open Error
open Utils.Position
open Utils.Buf

(* An opcode for the outputted p-code *)
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
  (* Instructions opcodes *)
  | OpcSet | OpcIfnset | OpcInit
  | OpcWait| OpcSpeed | OpcSend
  | OpcInvoke
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
  | OpcAccess
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
          of_opcode (OpcLiteral t);
        ]
      | LVec3 (x,y,z) -> concat [
          of_expression (value z);
          of_expression (value y);
          of_expression (value x);
          (* the literal type *)
          of_opcode (OpcLiteral t);
        ]
      (* Any other literal pushes constructor first and data later*)
      | _ ->
          concat [
              (* the literal type *)
              of_opcode (OpcLiteral t);
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
      of_expression (value rhs);
      of_expression (value lhs);
      (* the literal type *)
      of_opcode (OpcOperation (value op));
    ]
  (* A condition *)
  | ECondition (cond, thendo, elsedo) -> concat [
      (* push operands *)
      of_expression (value elsedo);
      of_expression (value thendo);
      of_expression (value cond);
      (* the literal type *)
      of_opcode OpcTernary;
    ]
  (* A function call *)
  | EFunc (identifier, arglist) -> concat
    (* all the expressions *)
    ((List.rev_map of_expression (List.map value (value arglist))) @ [
      (* the identifier *)
      of_scoped_identifier (SExtern, (value identifier));
      (* the opcode *)
      of_opcode OpcFunction;
    ])
  (* A cast *)
  | ETypeCast (t, expr) -> concat [
      (* push operand *)
      of_expression (value expr);
      (* push type *)
      of_opcode (OpcCast (value t))
    ]
  (* An access *)
  | EAccess (expr, id) -> concat [
      (* push operand *)
      of_expression (value expr);
      (* push id *)
      of_scoped_identifier (SLocal, (value id));
      (* push opcode *)
      of_opcode OpcAccess
    ]

(* Returns the p-code of an instruction *)
and of_instruction = function
  (* A nop instruction *)
  | INop -> of_opcode OpcNop
  (* A goto instruction *)
  | IGoto _ -> failwith "Unimplemented yet"
  (* A label instruction *)
  | ILabel _ -> failwith "Unimplemented yet"
  (* A variable set *)
  | ISet (scopedid, expr) -> concat [
      (* the expression *)
      of_expression (value expr);
      (* the scoped identifier*)
      of_scoped_identifier (value scopedid);
      (* the opcode*)
      (of_opcode OpcSet);
    ]
  (* A variable ifnet *)
  | IIfnset (scopedid, expr) -> concat [
      (* the expression *)
      of_expression (value expr);
      (* the scoped identifier*)
      of_scoped_identifier (value scopedid);
      (* the opcode*)
      (of_opcode OpcIfnset);
    ]
  (* A wait *)
  | IWait (id, expr) ->
    (* Return the buffer *)
    concat (match id with
      (* No id *)
      | None -> [
          (* the expression *)
          of_expression (value expr);
          (* the opcode *)
          of_opcode OpcWait
        ]
      (* An id.*)
      | Some i -> [
          (* the expression *)
          of_expression (value expr);
          (* the identifier *)
          of_scoped_identifier (SExtern, (value i));
          (* the opcode *)
          of_opcode OpcWait
        ]
    )
  (* A message *)
  | IMessage (msg) ->
    (* destruct *)
    let fstr, opts = (value msg) in
    (* Return the buffer *)
    concat [
        (* the fstring *)
        of_fstring fstr;
        (* the opcode *)
        (of_opcode (OpcMessage (List.rev_map value opts)));
      ]
  (* A choice *)
  | IChoice _ -> failwith "Unimplemented yet"

  (* A condition *)
  | ICondition (expr, branches) ->
    (* compute the skip length *)
    let skiplength = Buffer.length (of_opcode (OpcSkipIfNot (0L))) in
    (* generate pcode for a branch *)
    let of_branch branch isfirst =
      (* unlocate *)
      let pattern, prog = branch in
      (* match over pattern type*)
      begin match pattern with
        (* A wildcard, just the program *)
        | PWildcard -> (of_program prog, None)
        (* A value : generate the expression *)
        | PValue expr ->
          (* the pcode of the program *)
          let subprogram_pcode = of_program prog in
          (* Generate the p-code *)
          let pcodes = [
            (* matched value is previously on top of stack *)
            (* the expression value to match *)
            of_expression (value expr);
            (* the equal operator to test for equality *)
            of_opcode (OpcOperation OpEqual);
            (* if false, skip the program and the (goto end) *)
            of_opcode (OpcSkipIfNot (Int64.of_int ((Buffer.length subprogram_pcode) + skiplength + 2)));
              (* deepen the scope *)
              of_opcode (OpcDeepenScope);
              (* the program (skipped if false )*)
              subprogram_pcode;
              (* raise the scope *)
              of_opcode (OpcRaiseScope);
              (* goto end of condition, undefined for now (skipped in false )*)
              of_opcode (OpcSkip (0L))
          ] in
          (* if needed, duplicate the argument *)
          let pcode = if isfirst
                      then concat ((of_opcode OpcDupl)::pcodes)
                      else concat pcodes
          (* Return the pcode AND the offset at which the skipcode is found *)
          in (pcode, Some ((Buffer.length pcode) - 8))

        (* A binding pattern*)
        | PBinding (id, expr) ->
          (* the pcode of the program *)
          let subprogram_pcode = of_program prog in
          (* Generate the p-code *)
          let pcodes = [
            (* matched value is previously on top of stack *)
            (* deepen the scope to hide other variable definitions after *)
            of_opcode (OpcDeepenScope);
            (* locally initialize the identifier to the matched value (will fail if already bound)*)
            of_scoped_identifier (SLocal, (value id));
            of_opcode (OpcInit);
            (* the when statement *)
            of_expression (value expr);
            (* if false, skip the program and the goto end*)
            of_opcode (OpcSkipIfNot (Int64.of_int ((Buffer.length subprogram_pcode) + skiplength + 1)));
              (* the program *)
              subprogram_pcode;
              (* raise the scope *)
              of_opcode (OpcRaiseScope);
              (* goto end of condition, undefined for now *)
              of_opcode (OpcSkip (0L));
            (* raise the scope *)
            of_opcode (OpcRaiseScope);
          ] in
          (* if needed, duplicate the argument *)
          let pcode = if isfirst
                      then concat ((of_opcode OpcDupl)::pcodes)
                      else concat pcodes
          (* Return the pcode AND the offset at which the skipcode is found *)
          in (pcode, Some ((Buffer.length pcode) - 8 - 1 (* size of OPC_RAISE_SCOPE*)))
      end
    in
    (* compute the code of all branches *)
    let of_branches branches =
      (* cut a list of aug branches at first with no offset defined (meaning end)*)
      let rec cut_aug_branches acc = function
        (* empty : there was no nooffset branch, return accumulated*)
        | [] -> acc
        (* compute *)
        | (pat, prog)::tail ->
          (* get the code of branch *)
          let pcode, len = (of_branch ((value pat),(value prog)) (acc = [])) in
          (* match the len *)
          begin match len with
            | None -> (pcode, len)::acc
            | Some _ -> cut_aug_branches ((pcode, len)::acc) tail
          end
      in
      (* returns a list of the p-code of each branch and their byte size,
         in rev order, and modify the p-code in place *)
      let rec list_of_branches acc sum = function
        | [] -> acc
        | (pcode, offset_maybe)::t ->
          (* compute len *)
          let len = Buffer.length pcode in
          (* check if any offset*)
          begin match offset_maybe with
            (* No offset; just incorporate *)
            | None -> list_of_branches (pcode::acc) (sum+len) t
            (* Offset defined; modify to incorporate jumplist *)
            | Some offset ->
              (* to bytes *)
              let pcodebytes = Buffer.to_bytes pcode in
              (* compute the int64 buffer *)
              let skiplenbuf = int64_to_buf (Int64.of_int sum) in
              (* replace *)
              let () = Buffer.blit skiplenbuf 0 pcodebytes offset 8 in
              (* add code and size *)
              list_of_branches ((from_bytes pcodebytes)::acc) (sum+len) t
          end
      in
      (* compute the modified pcodes *)
      concat (list_of_branches [] 0 (cut_aug_branches [] branches))
    in
    (* concatenate everything *)
    concat [
      (* the matched expression *)
      of_expression (value expr);
      (* memorize it *)
      of_opcode OpcMem;
      (* the code of all branches *)
      of_branches branches
    ]
  (* An invoke *)
  | IInvoke (id, arglist) -> concat
    (* all the expressions *)
    ((List.rev_map of_expression (List.map value (value arglist))) @ [
      (* the identifier *)
      of_scoped_identifier (SExtern, (value id));
      (* the opcode *)
      of_opcode OpcInvoke;
    ])
  (* A speed command *)
  | ISpeed (expr) -> concat [
      (* the expression *)
      of_expression (value expr);
      (* the opcode *)
      of_opcode OpcSpeed
    ]
  (* A send command *)
  | ISend (id, expr) ->
    (* Return the buffer *)
    concat (match expr with
      (* No id *)
      | None -> [
          (* the identifier *)
          of_scoped_identifier (SExtern, (value id));
          (* the opcode *)
          of_opcode OpcSend
        ]
      (* An id.*)
      | Some e -> [
          (* the identifier *)
          of_scoped_identifier (SExtern, (value id));
          (* the expression *)
          of_expression (value e);
          (* the opcode *)
          of_opcode OpcSend
        ]
    )

  (* A declare command *)
  | IDeclare _ -> failwith "Unimplemented yet"

(* Returns the p-code of an opcode *)
and of_opcode opcode =
  (* get a byte list *)
  let rec bytelist_of_opcode = function
    (*
     * SPECIAL MARKERS
     *)
    (* End of data *)
    | OpcEOD -> [ 0x00 ]

    (*
     * FLOW CONTROL & STACK/ENV MANAGEMENT
     *)
     (* Stack management *)
    | OpcMem -> [0x01]
    | OpcDupl -> [0x02]
    | OpcDeepenScope -> [0x03]
    | OpcRaiseScope -> [0x04]
     (* Control flow *)
    | OpcSkipIfNot s -> 0x05::(byte_list_of_int64 s)
    | OpcSkip s -> 0x06::(byte_list_of_int64 s)
    (*
     * INSTRUCTIONS
     *)
    | OpcSet -> [ 0x20 ]
    | OpcIfnset -> [ 0x21 ]
    | OpcInit -> [ 0x22 ]
    | OpcMessage opts ->
      (* the function returning the flag value for a given option*)
      let message_opt_flag opt = match opt with
        | MsgNoRush -> 0x01
        | MsgNoAcknowledge -> 0x02
      (* Turn the list into a flag *)
      in 0x23::[(List.fold_left (+) 0 (List.rev_map message_opt_flag opts))]
    | OpcWait -> [ 0x24 ]
    | OpcSpeed -> [ 0x25 ]
    | OpcInvoke -> [ 0x26 ]
    | OpcSend -> [ 0x27 ]
    (* A nop *)
    | OpcNop -> [ 0x80 ]

    (*
     * IDENTIFIERS
     *)
    (* A variable or function (frozen) *)
    | OpcIdentifier scope -> begin match scope with
        | SExtern -> [ 0x60 ]
        | SGlobal -> [ 0x61 ]
        | SLocal -> [ 0x62 ]
      end

    (*
     * EXPRESSIONS
     *)
    (* A variable (accessed) *)
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
    (* Marks the last expression as inline expr *)
    | OpcInline -> [ 0x9F ]
    (* An operation *)
    | OpcOperation o -> 0xA0::begin match o with
      | OpPlus -> [ 0x00 ]
      | OpMinus -> [ 0x01 ]
      | OpStar -> [ 0x02 ]
      | OpDivide -> [ 0x03 ]
      | OpAnd -> [ 0x04 ]
      | OpOr -> [ 0x05 ]
      | OpEqual -> [ 0x06 ]
      | OpNotEqual -> [ 0x07 ]
      | OpLeq -> [ 0x08 ]
      | OpGeq -> [ 0x09 ]
      | OpLess -> [ 0x0A ]
      | OpMore -> [ 0x0B ]
    end
    (* A ternary token*)
    | OpcTernary -> [ 0xA1 ]
    (* A function call *)
    | OpcFunction -> [ 0xA2 ]
    (* A type cast *)
    | OpcCast t -> 0xA3::(bytelist_of_opcode (OpcLiteral t))
    (* An access *)
    | OpcAccess -> [ 0xA4 ]

  (* Return the p-code *)
  in from_byte_list (bytelist_of_opcode opcode)

(* Returns the p-code of a scoped identifier*)
and of_scoped_identifier scopedid =
  (* destruct *)
  let scope, id = scopedid in
  (* Make default buffer *)
  let buf = (of_opcode (OpcIdentifier scope)) in
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
      Buffer.add_buffer buffer ((of_opcode OpcEOD));
      (* Return *)
      concat [
          (* the value type *)
          (of_opcode (OpcLiteral TString));
          (* the contents *)
          buffer
      ]
    (* An inline expr *)
    | StrInline e ->
      (* return the buffer for the expression and add a inline marker *)
      concat [
        of_expression (value e);
        (of_opcode OpcInline)
      ]
    (*  *)
    | StrColor _ -> assert false
  in
  (* glue the tokens together *)
  let rec fstring_glue fstr = match fstr with
    (* Empty fstring *)
    | [] -> concat [
        (* the value type *)
        (of_opcode (OpcLiteral TString));
        (* the contents *)
        (of_opcode (OpcEOD))
    ]
    (* Single token : good on its own *)
    | [t] -> of_fstring_tok (value t)
    (* Two tokens : put then glue *)
    | [t1; t2] -> concat [
        (* the tokens *)
        of_fstring_tok (value t2);
        of_fstring_tok (value t1);
        (* the glue *)
        (of_opcode (OpcOperation OpPlus));
    ]
    (* More : glue and recursively call*)
    | t::tail -> concat [

      fstring_glue tail;
      of_fstring_tok (value t);
      (* the glue *)
      (of_opcode (OpcOperation OpPlus));
    ]
  in
  (* concat the pcode for each token *)
  fstring_glue fstr

(* Returns the p-code of an expression variable *)
and of_variable scopedid =
  (* destruct *)
  let scope, id = scopedid in
  (* Make default buffer *)
  let buf = (of_opcode (OpcVariable scope)) in
  (* Add the string and terminate *)
  write_identifier buf id;
  (* Return the buffer *)
  buf

(* Write identifier in the buffer *)
and write_identifier buf id = match id with
  | Id s -> Buffer.add_string buf s;
  (* Terminate *)
  Buffer.add_buffer buf ((of_opcode OpcEOD))


(* Writes to an output channel *)
let to_out channel pcode =
  (* Write *)
  Buffer.output_buffer channel pcode
