open DLG.Ast
open Error
open Utils.Position


(* Build the jump table of the program *)
let rec add_jump_table pcode =
  (* the jump table *)
  let jump_table = ref [] in
  (* the jump label lists  *)
  let jump_label = Hashtbl.create 16 in
  (* the scope depth counter *)
  let scope_depth = ref 0l in
  let label_fresh = ref 0l in
  let current_count = ref 0L in
  (* iterate through list, register each label and count scope *)
  let pcode_register = function
    (* check if opc is label, and count as a nop *)
    | Pcode.OpcLabelId ident ->
      (* bind it to table *)
      Hashtbl.replace jump_label ident (!label_fresh, !scope_depth, !current_count);
      (* add into jump table *)
      jump_table := ((!scope_depth, !current_count))::(!jump_table);
      (* make a fresh new label *)
      label_fresh := Int32.succ (!label_fresh);
      (* count *)
      current_count := Int64.add (!current_count) (Pcode.byte_length_of (of_opcode Pcode.OpcNop));
    (* check if opc is goto, and count as a labeled goto instead of identified *)
    | Pcode.OpcGotoId _ ->
      (* count *)
      current_count := Int64.add (!current_count) (Pcode.byte_length_of (of_opcode (Pcode.OpcGoto 0l)));
    (* else *)
    | opc ->
      (* measure depth *)
      if opc = Pcode.OpcDeepenScope then scope_depth := Int32.succ !scope_depth;
      if opc = Pcode.OpcRaiseScope then scope_depth := Int32.pred !scope_depth;
      (* count *)
      current_count := Int64.add (!current_count) (Pcode.byte_length_of (of_opcode opc));
  in
  (* now replace the gotos; *)
  let pcode_replace _ = function
    (* check if opc is goto *)
    | Pcode.OpcGotoId ident ->
      (* try get definition in previously made table *)
      let label, _, _ = match Hashtbl.find_opt jump_label ident with
        | None -> raise (Compile_error { reason = CompileErrorGotoUnbound })
        | Some d -> d
      in
      (* replace by a labeled goto *)
      Pcode.OpcGoto label
    (* check if opc is label, replace by nop *)
    | Pcode.OpcLabelId _ -> Pcode.OpcNop
    (* else, do nothing *)
    | opc -> opc
  in
  (* offset the jump-table indices (src is reversed) *)
  let offset_jump_table offset (scope, count) =
    of_opcode (Pcode.OpcJump (scope, (Int64.add count offset)))
  in

  (* iterate *)
  Pcode.iter pcode pcode_register;
  (* replace instrs *)
  let pcode_edited = Pcode.mapi pcode pcode_replace in
  (* the offset, assuming a jump opcode is of constant size (it is),
     is the size of a jump*nbr_jumps + 1
  *)
  let jump_table_offset_bytes =
    Int64.add
      1L
      (Int64.mul
        (Pcode.byte_length_of (of_opcode (Pcode.OpcJump (0l, 0L))))
        (Int64.of_int (List.length !jump_table)))
  in
  (* offset the jump table *)
  let pcode_jump_table = Pcode.concat (List.rev_map (offset_jump_table jump_table_offset_bytes) !jump_table) in
  (* return the jump table *)
  Pcode.concat [pcode_jump_table; of_opcode (OpcJumpTableEnd); pcode_edited]

(* Returns the p-code of a program *)
and compile program =
  (* Compute the p_code *)
  let pcode = of_subprogram program in
  (* build the jump table *)
  let pcode_with_jumptable = add_jump_table pcode in
  (* Collect the GC. Woohoo *)
  Gc.compact ();
  (* Return the p-code *)
  pcode_with_jumptable

(* Returns the p-code of a subprogram *)
and of_subprogram program =
  (* the current program insctruction and the rest of the program *)
  let program_instr = ref INop in
  let program_left = ref program in
  (* create the pcode *)
  let program_code = ref (Pcode.empty ()) in
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
        program_code := Pcode.append !program_code (of_instruction !program_instr)
    end;
  done;
  (* return the buffer *)
  !program_code

(* Returns the p_code of an opcode *)
and of_opcode opcode = Pcode.of_list [ opcode ]
(* Returns the p_code of an instruction *)
and of_instruction = function
  (* A nop *)
  | INop -> of_opcode OpcNop
  (* A goto/label *)
  | IGoto id -> of_opcode (OpcGotoId (value id))
  | ILabel id -> of_opcode (OpcLabelId (value id))
  (* A set/ifnset operation *)
  | ISet (sid, expr) -> Pcode.concat [
      of_expression (value expr);
      of_opcode (OpcIdentifier (value sid));
      of_opcode OpcSet;
    ]
  | IIfnset (sid, expr) -> Pcode.concat [
      of_expression (value expr);
      of_opcode (OpcIdentifier (value sid));
      of_opcode OpcIfnset;
    ]
  (* A wait instruction *)
  | IWait (id, expr) ->
    (* Return the code *)
    Pcode.concat (match id with
      (* No id *)
      | None -> [
          of_expression (value expr);
          of_opcode OpcWait;
        ]
      (* An id.*)
      | Some i -> [
          of_expression (value expr);
          of_opcode (OpcIdentifier (SExtern, value i));
          of_opcode OpcWait;
        ]
    )
  (* A message *)
  | IMessage (msg) ->
    (* destruct *)
    let fstr, opts = (value msg) in
    (* Return the buffer *)
    Pcode.concat [
        (* the fstring *)
        of_fstring fstr;
        (* the opcode *)
        (of_opcode (OpcMessage (List.rev_map value opts)));
      ]

  (* An invoke *)
  | IInvoke (id, arglist) -> Pcode.concat
    (* all the expressions *)
    ((List.rev_map of_expression (List.map value (value arglist))) @ [
      of_opcode (OpcIdentifier (SExtern, value id));
      of_opcode (OpcInvoke (Int32.of_int (List.length (value arglist))));
    ])
  (* A speed command *)
  | ISpeed (expr) -> Pcode.concat [
      of_expression (value expr);
      of_opcode OpcSpeed;
    ]
  (* A send command *)
  | ISend (id, expr) ->
    (* Return the buffer *)
    Pcode.concat (match expr with
      (* No id *)
      | None -> [
          of_opcode (OpcIdentifier (SExtern, value id));
          of_opcode OpcSend;
        ]
      (* An id.*)
      | Some e -> [
          of_opcode (OpcIdentifier (SExtern, value id));
          of_expression (value e);
          of_opcode OpcSend;
        ]
    )
  (* A declare command *)
  | IDeclare _ -> Pcode.empty ()
  (* A choice *)
  | IChoice choices ->
    (* the number of choices *)
    let choices_num = List.length choices in
    (* get the p-code of all strings in the choice *)
    let pcode_choices_str = Pcode.concat (List.rev_map (fun c -> of_fstring (value (fst c))) choices) in
    (* the the pcode of a choice *)
    let of_choice choice i isfirst =
      (* destruct *)
      let _, prog = choice in
      (* the pcode of the branch *)
      let branch_pcode = Pcode.concat [
        (* deepen the scope *)
        of_opcode (OpcDeepenScope);
        (* the program (skipped if false )*)
        of_subprogram (value prog);
        (* raise the scope *)
        of_opcode (OpcRaiseScope);
        (* goto end of condition, undefined for now (skipped in false )*)
        of_opcode (OpcSkip (0L))
      ] in
      (* Generate the p-code *)
      let pcodes = [
        (* checked id is previously on top of stack *)
        (* test equality with current choice *)
        of_literal (LInt i);
        of_opcode (OpcOperation OpEqual);
        (* if false, skip the program and the (goto end) *)
        of_opcode (OpcSkipIfNot (Pcode.byte_length_of branch_pcode));
        (* the branch code *)
        branch_pcode;
      ] in
      (* if needed, duplicate the argument *)
      let pcode = if not isfirst
                  then Pcode.concat ((of_opcode OpcDupl)::pcodes)
                  else Pcode.concat pcodes
      (* Return the pcode AND the offset at which the skipcode is found *)
      in (pcode, Int64.sub (Pcode.length pcode) 1L)
    in
    (* takes a list of the p-code of each choice and their byte size,
       in rev order, and modify the p-code in place *)
    let rec list_of_choices acc sum i = function
      | [] -> acc
      | choice::t ->
        (* deconstruct *)
        let pcode, poffset = of_choice choice (Int32.of_int i) (t=[]) in
        (* replace in place *)
        let replacer idx opc =
          (* check if idx maches *)
          if Int64.of_int idx <> poffset then opc
          (* matches. replace *)
          else match opc with
            | Pcode.OpcSkip _ -> Pcode.OpcSkip sum
            | _ -> assert false
        in
        (* replace *)
        let pcode_edited = Pcode.mapi pcode replacer in
        (* compute len *)
        let len = Pcode.byte_length_of pcode in
        (* add code and size *)
        list_of_choices (pcode_edited::acc) (Int64.add sum len) (i-1) t
    in
    (* compute the modified pcodes *)
    let pcode_choices = Pcode.concat (list_of_choices [] 0L (choices_num - 1) (List.rev choices)) in
    (* return the buffer *)
    Pcode.concat [
      (* the text for the choices *)
      pcode_choices_str;
      (* the number of choices *)
      of_literal (LInt (Int32.of_int choices_num));
      (* the choice instruction : wait for user token *)
      of_opcode OpcChoice;
      (* the user will push an integer token on the stack here *)
      (* memorize it *)
      of_opcode OpcMem;
      (* the choices *)
      pcode_choices;
    ]
  (* A condition *)
  | ICondition (expr, branches) ->
    (* generate pcode for a branch *)
    let of_branch branch isfirst =
      (* unlocate *)
      let pattern, prog = branch in
      (* match over pattern type*)
      begin match pattern with
        (* A wildcard, just the program *)
        | PWildcard -> (Pcode.concat [
            (* deepen the scope *)
            of_opcode (OpcDeepenScope);
            (* the program*)
            of_subprogram prog;
            (* raise the scope *)
            of_opcode (OpcRaiseScope);
          ], None)
        (* A value : generate the expression *)
        | PValue expr ->
          (* the pcode of the branch *)
          let branch_pcode = Pcode.concat [
            (* deepen the scope *)
            of_opcode (OpcDeepenScope);
            (* the program (skipped if false )*)
            of_subprogram prog;
            (* raise the scope *)
            of_opcode (OpcRaiseScope);
            (* goto end of condition, undefined for now (skipped in false )*)
            of_opcode (OpcSkip (0L))
          ] in
          (* Generate the p-code *)
          let pcodes = [
            (* matched value is previously on top of stack *)
            (* the expression value to match *)
            of_expression (value expr);
            (* the equal operator to test for equality *)
            of_opcode (OpcOperation OpEqual);
            (* if false, skip the program and the (goto end) *)
            of_opcode (OpcSkipIfNot (Pcode.byte_length_of branch_pcode));
            (* deepen the scope *)
            branch_pcode;
          ] in
          (* if needed, duplicate the argument *)
          let pcode = if not isfirst
                      then Pcode.concat ((of_opcode OpcDupl)::pcodes)
                      else Pcode.concat pcodes
          (* Return the pcode AND the offset at which the skipcode is found *)
          in (pcode, Some (Int64.sub (Pcode.length pcode) 1L))

        (* A binding pattern*)
        | PBinding (id, expr) ->
          (* the pcode of the branch *)
          let branch_pcode = Pcode.concat [
            (* deepen the scope to hide other variable definitions after *)
            of_opcode (OpcDeepenScope);
            (* define captured variable in deepened scope *)
            of_opcode (OpcDupl);
            of_opcode (OpcIdentifier (SLocal, (value id)));
            of_opcode (OpcInit);
            (* the program (skipped if false )*)
            of_subprogram prog;
            (* raise the scope *)
            of_opcode (OpcRaiseScope);
            (* goto end of condition, undefined for now (skipped in false )*)
            of_opcode (OpcSkip (0L))
          ] in
          (* Generate the p-code *)
          let pcodes = [
            (* deepen the scope to hide other variable definitions after *)
            of_opcode (OpcDeepenScope);
            (* matched value is previously on top of stack *)
            (* locally initialize the identifier to the matched value (will fail if already bound)*)
            of_opcode (OpcIdentifier (SLocal, (value id)));
            of_opcode (OpcInit);
            (* the when statement *)
            of_expression (value expr);
            (* raise the scope *)
            of_opcode (OpcRaiseScope);
            (* if false, skip the program and the goto end*)
            of_opcode (OpcSkipIfNot (Pcode.byte_length_of branch_pcode));
              (* the program *)
              branch_pcode;
          ] in
          (* if needed, duplicate the argument *)
          let pcode = if not isfirst
                      then Pcode.concat ((of_opcode OpcDupl)::pcodes)
                      else Pcode.concat pcodes
          (* Return the pcode AND the offset at which the skipcode is found *)
          in (pcode, Some (Int64.sub (Pcode.length pcode) 1L))
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
          let pcode, plen = (of_branch ((value pat),(value prog)) (acc = [])) in
          (* match the len *)
          begin match plen with
            | None -> (pcode, plen)::acc
            | Some _ -> cut_aug_branches ((pcode, plen)::acc) tail
          end
      in
      (* returns a list of the p-code of each branch and their byte size,
         in rev order, and modify the p-code in place *)
      let rec list_of_branches acc sum = function
        | [] -> acc
        | (pcode, offset_maybe)::t ->
          (* compute len *)
          let len = Pcode.byte_length_of pcode in
          (* check if any offset*)
          begin match offset_maybe with
            (* No offset; just incorporate *)
            | None -> list_of_branches (pcode::acc) (Int64.add sum len) t
            (* Offset defined; modify to incorporate jumplist *)
            | Some poffset ->
              (* replace in place *)
              let replacer idx opc =
                (* check if idx maches *)
                if Int64.of_int idx <> poffset then opc
                (* matches. replace *)
                else match opc with
                  | Pcode.OpcSkip _ -> Pcode.OpcSkip sum
                  | _ -> assert false
              in
              (* replace *)
              let pcode_edited = Pcode.mapi pcode replacer in
              (* add code and size *)
              list_of_branches (pcode_edited::acc) (Int64.add sum len) t
          end
      in
      (* compute the modified pcodes *)
      Pcode.concat (list_of_branches [] 0L (cut_aug_branches [] branches))
    in
    (* concatenate everything *)
    Pcode.concat [
      (* the matched expression *)
      of_expression (value expr);
      (* memorize it *)
      of_opcode OpcMem;
      (* the code of all branches *)
      of_branches branches;
    ]

(* Returns the p_code of an expression *)
and of_expression expr = match expr with
  (* A variable *)
  | EVar scopedid -> Pcode.concat [
      of_opcode (OpcIdentifier (value scopedid));
      of_opcode OpcVariable;
    ]
  (* A literal *)
  | ELiteral literal -> of_literal (value literal)
  (* An operation *)
  | EOperation (op, lhs, rhs) -> Pcode.concat [
      of_expression (value rhs);
      of_expression (value lhs);
      of_opcode (OpcOperation (value op));
    ]
  (* An unary operation *)
  | EUnaryOperation (op, v) -> Pcode.concat [
      of_expression (value v);
      of_opcode (OpcUnaryOperation (value op));
    ]
  (* A condition *)
  | ECondition (cond, thendo, elsedo) -> Pcode.concat [
      of_expression (value elsedo);
      of_expression (value thendo);
      of_expression (value cond);
      of_opcode OpcTernary;
    ]
  (* A function call *)
  | EFunc (identifier, arglist) -> Pcode.concat
    (* all the expressions *)
    ((List.rev_map of_expression (List.map value (value arglist))) @ [
      of_opcode (OpcIdentifier (SExtern, value identifier));
      of_opcode (OpcFunctionCall (Int32.of_int (List.length (value arglist))));
    ])
  (* A cast *)
  | ETypeCast (t, expr) -> Pcode.concat [
      of_expression (value expr);
      of_opcode (OpcCast (value t))
    ]
  (* An access *)
  | EAccess (expr, id) -> Pcode.concat [
      of_expression (value expr);
      of_opcode (OpcIdentifier (SLocal, value id));
      of_opcode OpcAccess;
    ]

(* Returns the p_code of a literal *)
and of_literal literal = match literal with
  (* vectors push expressions first and constructor later *)
  | LVec2 (x,y) -> Pcode.concat [
      of_expression (value y);
      of_expression (value x);
      of_opcode (OpcValue VVec2);
    ]
  | LVec3 (x,y,z) -> Pcode.concat [
      of_expression (value z);
      of_expression (value y);
      of_expression (value x);
      of_opcode (OpcValue VVec3);
    ]
  (* Any other literal*)
  | LInt i -> of_opcode (OpcValue (VInt i))
  | LFloat f -> of_opcode (OpcValue (VFloat f))
  | LBool b -> of_opcode (OpcValue (VBool b))
  | LString fstr -> of_fstring fstr
  | LEnum (t, v) -> of_opcode (OpcValue (VEnum (value t, value v)))

(* Returns the p_code of a string *)
and of_fstring fstr =
  (* outputs the pcode for a token *)
  let of_fstring_tok = function
    (* A constant string *)
    | StrConst s -> of_opcode (OpcValue (VString s))
    (* An inline expr *)
    | StrInline e ->
      (* return the buffer for the expression and add a inline marker *)
      Pcode.concat [
        of_expression (value e);
        of_opcode OpcInline;
      ]
    (* A color *)
    | StrColor _ -> assert false
  in
  (* glue the tokens together *)
  let rec fstring_glue fstr = match fstr with
    (* Empty fstring *)
    | [] -> of_opcode (OpcValue (VString ""))
    (* Single token : good on its own *)
    | [t] -> of_fstring_tok (value t)
    (* Two tokens : put then glue *)
    | [t1; t2] -> Pcode.concat [
        of_fstring_tok (value t2);
        of_fstring_tok (value t1);
        of_opcode (OpcOperation OpPlus);
      ]
    (* More : glue and recursively call*)
    | t::tail -> Pcode.concat [
        (* glue the tail *)
        fstring_glue tail;
        of_fstring_tok (value t);
        (of_opcode (OpcOperation OpPlus));
      ]
  in
  (* concat the pcode for each token *)
  fstring_glue fstr
