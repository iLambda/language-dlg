open DLG.Ast
open Utils.Position
open Typing.Type

(* Raised when there was a failure in optimising something *)
exception Optimisation_failure
(* Raised in order to take a shortcut a always used branch in the optimisation process *)
exception Optimisation_always_used_branch of program
(* Raised when a wildcard pattern is matched so it cuts all the branches after *)
exception Optimisation_unused_branches_after of pattern

(* Optimizes an expression *)
let rec optimise_expr expr = match expr with

  (* Optimize a literal *)
  | ELiteral lit ->
    (* Check the type *)
    begin match value lit with
      (* A 2D vector *)
      | LVec2 (x, y) ->
        (* optimize x and y *)
        let optx = map optimise_expr x in
        let opty = map optimise_expr y in
        (* rebuild *)
        ELiteral (unknown_pos (LVec2 (optx, opty)))
      (* A 3D vector *)
      | LVec3 (x, y, z) ->
        (* optimize x, y and z *)
        let optx = map optimise_expr x in
        let opty = map optimise_expr y in
        let optz = map optimise_expr z in
        (* rebuild *)
        ELiteral (unknown_pos (LVec3 (optx, opty, optz)))
      (* A formatted string *)
      | LString fstr ->
        (* rebuild *)
        ELiteral (unknown_pos (LString (optimise_fstring fstr)))
      (* Cannot optimize *)
      | _ -> expr
    end

  (* Optimize a condition *)
  | ECondition (condition, thendo, elsedo) ->
    (* Try optimize condition*)
    let optcondition = optimise_expr (value condition) in
    (* If it is a literal, resolve directly *)
    begin match optcondition with
      (* It is a boolean literal. Return the right branch *)
      | ELiteral { value=(LBool b); _ } ->
        (* It's true, return first branch *)
        if b then optimise_expr (value thendo)
             else optimise_expr (value elsedo)
      (* Can't tell more. Optimize branches *)
      | _ ->
        (* Optimize all *)
        let optthen = map optimise_expr thendo in
        let optelse = map optimise_expr elsedo in
        (* Return *)
        ECondition ((unknown_pos optcondition), optthen, optelse)
    end

  (* Optimize constructed type access *)
  | EAccess (constr, id) ->
    (* Optimize the constructed value *)
    let optconstr = optimise_expr (value constr) in
    (* Gets the string represented by the ID *)
    let accessed = match (value id) with Id s -> s in
    (* Check the property *)
    begin match accessed with
      (* Check vector x field access *)
      | "x" -> begin match optconstr with
        (* vectors *)
        | ELiteral { value=(LVec2(({ value=x; _ }), _)); _ }
        | ELiteral { value=(LVec3(({ value=x; _ }), _, _)); _ } ->
          (* Create a cast and optimize it *)
          optimise_expr (ETypeCast (unknown_pos TFloat, unknown_pos (optimise_expr x)))
        (* Anything else : can't optimize more *)
        | _ -> EAccess ((unknown_pos optconstr), id)
        end
      (* Check vector y field access *)
      | "y" -> begin match optconstr with
        (* vectors *)
        | ELiteral { value=(LVec2(_, ({ value=y; _ }))); _ }
        | ELiteral { value=(LVec3(_, ({ value=y; _ }), _)); _ } ->
          (* Create a cast and optimize it *)
          optimise_expr (ETypeCast (unknown_pos TFloat, unknown_pos (optimise_expr y)))
        (* Anything else : can't optimize more *)
        | _ -> EAccess ((unknown_pos optconstr), id)
        end
      (* Check vector y field access *)
      | "z" -> begin match optconstr with
        (* vectors *)
        | ELiteral { value=(LVec3(_, _, ({ value=z; _ }))); _ } ->
          (* Create a cast and optimize it *)
          optimise_expr (ETypeCast (unknown_pos TFloat, unknown_pos (optimise_expr z)))
        (* Anything else : can't optimize more *)
        | _ -> EAccess ((unknown_pos optconstr), id)
        end
      (* No optimizable property. *)
      | _ -> EAccess ((unknown_pos optconstr), id)
    end

  (* Optimize a type cast *)
  | ETypeCast (totype, tocast) ->
    (* Optimize the expression *)
    let optexpr = optimise_expr (value tocast) in
    (* Match over target type and optexpr *)
    begin match optexpr, (value totype) with
      (* if we have a literal and a type that's the same *)
      | ELiteral { value=lit; _ }, t when type_is_same (type_of_literal lit) t ->
        (* Return the literal itself since it has right type *)
        optexpr
      (* if we have a literal and a type that's not the same *)
      | ELiteral { value=lit; position=p }, t ->
        (* Actually do cast (never throws since type checking worked )*)
        ELiteral (with_pos p (type_cast_literal t lit))
      (* if we don't have a literal, don't optimize *)
      | _ -> expr
    end

  (* Optimize an operation *)
  | EOperation (op, lhs, rhs) ->
    (* Optimize lhs and rhs *)
    let optlhs = optimise_expr (value lhs) in
    let optrhs = optimise_expr (value rhs) in
    (* Match over the two*)
    begin match optlhs, optrhs with
      (* Two literals. Compute *)
      | ELiteral { value=llit; _ }, ELiteral { value=rlit; _ } ->
        (* Try to compute them *)
        let result = try Some (optimise_operation llit (value op) rlit)
                     with Optimisation_failure -> None in
        (* Check if computation worked *)
        begin match result with
          (* Can't optimize more *)
          | None -> EOperation(op, unknown_pos optlhs, unknown_pos optrhs)
          (* Return the computation *)
          | Some r -> ELiteral (unknown_pos r)
        end
      (* Anything else ; can't optimize more *)
      | _ -> EOperation(op, unknown_pos optlhs, unknown_pos optrhs)
    end


  (* Ignore ; can't optimize *)
  | _ -> expr


(* Optimizes a computation between literals *)
and optimise_operation lhs op rhs = match op with
  (* + operator *)
  | OpPlus -> begin match lhs, rhs with
      (* numbers *)
      | LInt l, LInt r -> LInt (Int32.add l r)
      | LFloat l, LInt r -> LFloat (l +. (Int32.to_float r))
      | LInt l, LFloat r -> LFloat ((Int32.to_float l) +. r)
      | LFloat l, LFloat r -> LFloat (l +. r)

      (* formatted strings *)
      | LString l, LString r -> LString (optimise_fstring (l @ r))

      (* 2D vector addition *)
      | LVec2 (lx, ly), LVec2 (rx, ry) ->
        (* construct optimized sums expressions *)
        let x = optimise_expr (EOperation (unknown_pos op, lx, rx)) in
        let y = optimise_expr (EOperation (unknown_pos op, ly, ry)) in
        (* return composition *)
        LVec2 (unknown_pos x, unknown_pos y)

      (* 3D vector addition *)
      | LVec3 (lx, ly, lz), LVec3 (rx, ry, rz) ->
        (* construct optimized sums expressions *)
        let x = optimise_expr (EOperation (unknown_pos op, lx, rx)) in
        let y = optimise_expr (EOperation (unknown_pos op, ly, ry)) in
        let z = optimise_expr (EOperation (unknown_pos op, lz, rz)) in
        (* return composition *)
        LVec3 (unknown_pos x, unknown_pos y, unknown_pos z)

      (* Impossible because of typecheck*)
      | _ -> assert false
      end

  (* + operator *)
  | OpMinus -> begin match lhs, rhs with
      (* numbers *)
      | LInt l, LInt r -> LInt (Int32.sub l r)
      | LFloat l, LInt r -> LFloat (l -. (Int32.to_float r))
      | LInt l, LFloat r -> LFloat ((Int32.to_float l) -. r)
      | LFloat l, LFloat r -> LFloat (l -. r)

      (* 2D vector addition *)
      | LVec2 (lx, ly), LVec2 (rx, ry) ->
        (* construct optimized sums expressions *)
        let x = optimise_expr (EOperation (unknown_pos op, lx, rx)) in
        let y = optimise_expr (EOperation (unknown_pos op, ly, ry)) in
        (* return composition *)
        LVec2 (unknown_pos x, unknown_pos y)

      (* 3D vector addition *)
      | LVec3 (lx, ly, lz), LVec3 (rx, ry, rz) ->
        (* construct optimized sums expressions *)
        let x = optimise_expr (EOperation (unknown_pos op, lx, rx)) in
        let y = optimise_expr (EOperation (unknown_pos op, ly, ry)) in
        let z = optimise_expr (EOperation (unknown_pos op, lz, rz)) in
        (* return composition *)
        LVec3 (unknown_pos x, unknown_pos y, unknown_pos z)

      (* Impossible because of typecheck*)
      | _ -> assert false
      end

  (* * operator *)
  | OpStar -> begin match lhs, rhs with
      (* numbers *)
      | LInt l, LInt r -> LInt (Int32.mul l r)
      | LFloat l, LInt r -> LFloat (l *. (Int32.to_float r))
      | LInt l, LFloat r -> LFloat ((Int32.to_float l) *. r)
      | LFloat l, LFloat r -> LFloat (l *. r)

      (* string repetition *)
      | LString fstr, LInt i
      | LInt i, LString fstr ->
        (* a helper *)
        let rec repeat acc n l = match n with
          | i when i <= 0l -> acc
          | _ -> repeat (optimise_fstring (l @ acc)) (Int32.sub n 1l) l
        (* reconstruct optimized *)
        in LString (repeat [] i (optimise_fstring fstr))

      (* 2D vector multiplication *)
      | LVec2 (x, y), LInt i
      | LInt i, LVec2 (x, y) ->
        (* construct dummy expression for i *)
        let iexpr = ELiteral (unknown_pos (LInt i)) in
        (* construct optimized sums expressions *)
        let x = optimise_expr (EOperation (unknown_pos op, x, unknown_pos iexpr)) in
        let y = optimise_expr (EOperation (unknown_pos op, y, unknown_pos iexpr)) in
        (* return composition *)
        LVec2 (unknown_pos x, unknown_pos y)
      | LVec2 (x, y), LFloat f
      | LFloat f, LVec2 (x, y) ->
        (* construct dummy expression for i *)
        let iexpr = ELiteral (unknown_pos (LFloat f)) in
        (* construct optimized sums expressions *)
        let x = optimise_expr (EOperation (unknown_pos op, x, unknown_pos iexpr)) in
        let y = optimise_expr (EOperation (unknown_pos op, y, unknown_pos iexpr)) in
        (* return composition *)
        LVec2 (unknown_pos x, unknown_pos y)

      (* 3D vector subtraction *)
      | LVec3 (x, y, z), LInt i
      | LInt i, LVec3 (x, y, z) ->
        (* construct dummy expression for i *)
        let iexpr = ELiteral (unknown_pos (LInt i)) in
        (* construct optimized sums expressions *)
        let x = optimise_expr (EOperation (unknown_pos op, x, unknown_pos iexpr)) in
        let y = optimise_expr (EOperation (unknown_pos op, y, unknown_pos iexpr)) in
        let z = optimise_expr (EOperation (unknown_pos op, z, unknown_pos iexpr)) in
        (* return composition *)
        LVec3 (unknown_pos x, unknown_pos y, unknown_pos z)

      | LVec3 (x, y, z), LFloat f
      | LFloat f, LVec3 (x, y, z) ->
        (* construct dummy expression for i *)
        let iexpr = ELiteral (unknown_pos (LFloat f)) in
        (* construct optimized sums expressions *)
        let x = optimise_expr (EOperation (unknown_pos op, x, unknown_pos iexpr)) in
        let y = optimise_expr (EOperation (unknown_pos op, y, unknown_pos iexpr)) in
        let z = optimise_expr (EOperation (unknown_pos op, z, unknown_pos iexpr)) in
        (* return composition *)
        LVec3 (unknown_pos x, unknown_pos y, unknown_pos z)

      (* Impossible because of typecheck*)
      | _ -> assert false
      end


  (* / operator *)
  | OpDivide -> begin match lhs, rhs with
      (* numbers *)
      | LInt l, LInt r -> LInt (Int32.div l r)
      | LFloat l, LInt r -> LFloat (l /. (Int32.to_float r))
      | LInt l, LFloat r -> LFloat ((Int32.to_float l) /. r)
      | LFloat l, LFloat r -> LFloat (l /. r)

      (* 2D vector multiplication *)
      | LVec2 (x, y), LInt i
      | LInt i, LVec2 (x, y) ->
        (* construct dummy expression for i *)
        let iexpr = ELiteral (unknown_pos (LInt i)) in
        (* construct optimized sums expressions *)
        let x = optimise_expr (EOperation (unknown_pos op, x, unknown_pos iexpr)) in
        let y = optimise_expr (EOperation (unknown_pos op, y, unknown_pos iexpr)) in
        (* return composition *)
        LVec2 (unknown_pos x, unknown_pos y)
      | LVec2 (x, y), LFloat f
      | LFloat f, LVec2 (x, y) ->
        (* construct dummy expression for i *)
        let iexpr = ELiteral (unknown_pos (LFloat f)) in
        (* construct optimized sums expressions *)
        let x = optimise_expr (EOperation (unknown_pos op, x, unknown_pos iexpr)) in
        let y = optimise_expr (EOperation (unknown_pos op, y, unknown_pos iexpr)) in
        (* return composition *)
        LVec2 (unknown_pos x, unknown_pos y)

      (* 3D vector subtraction *)
      | LVec3 (x, y, z), LInt i
      | LInt i, LVec3 (x, y, z) ->
        (* construct dummy expression for i *)
        let iexpr = ELiteral (unknown_pos (LInt i)) in
        (* construct optimized sums expressions *)
        let x = optimise_expr (EOperation (unknown_pos op, x, unknown_pos iexpr)) in
        let y = optimise_expr (EOperation (unknown_pos op, y, unknown_pos iexpr)) in
        let z = optimise_expr (EOperation (unknown_pos op, z, unknown_pos iexpr)) in
        (* return composition *)
        LVec3 (unknown_pos x, unknown_pos y, unknown_pos z)

      | LVec3 (x, y, z), LFloat f
      | LFloat f, LVec3 (x, y, z) ->
        (* construct dummy expression for i *)
        let iexpr = ELiteral (unknown_pos (LFloat f)) in
        (* construct optimized sums expressions *)
        let x = optimise_expr (EOperation (unknown_pos op, x, unknown_pos iexpr)) in
        let y = optimise_expr (EOperation (unknown_pos op, y, unknown_pos iexpr)) in
        let z = optimise_expr (EOperation (unknown_pos op, z, unknown_pos iexpr)) in
        (* return composition *)
        LVec3 (unknown_pos x, unknown_pos y, unknown_pos z)
      (* Impossible because of typecheck*)
      | _ -> assert false
      end

  (* and operator *)
  | OpAnd -> begin match lhs, rhs with
      (* numbers *)
      | LBool l, LBool r -> LBool (l && r)
      (* Impossible because of typecheck*)
      | _ -> assert false
      end

  (* or operator *)
  | OpOr -> begin match lhs, rhs with
      (* numbers *)
      | LBool l, LBool r -> LBool (l || r)
      (* Impossible because of typecheck*)
      | _ -> assert false
      end

  (* comparison operators *)
  | OpLeq -> begin match lhs, rhs with
      (* numbers *)
      | LInt l, LInt r -> LBool (l <= r)
      | LFloat l, LInt r -> LBool (l <= (Int32.to_float r))
      | LInt l, LFloat r -> LBool ((Int32.to_float l) <= r)
      | LFloat l, LFloat r -> LBool (l <= r)
      (* Impossible because of typecheck*)
      | _ -> assert false
      end
  | OpGeq -> begin match lhs, rhs with
      (* numbers *)
      | LInt l, LInt r -> LBool (l >= r)
      | LFloat l, LInt r -> LBool (l >= (Int32.to_float r))
      | LInt l, LFloat r -> LBool ((Int32.to_float l) >= r)
      | LFloat l, LFloat r -> LBool (l >= r)
      (* Impossible because of typecheck*)
      | _ -> assert false
      end
  | OpLess -> begin match lhs, rhs with
      (* numbers *)
      | LInt l, LInt r -> LBool (l < r)
      | LFloat l, LInt r -> LBool (l < (Int32.to_float r))
      | LInt l, LFloat r -> LBool ((Int32.to_float l) < r)
      | LFloat l, LFloat r -> LBool (l < r)
      (* Impossible because of typecheck*)
      | _ -> assert false
      end
  | OpMore -> begin match lhs, rhs with
      (* numbers *)
      | LInt l, LInt r -> LBool (l > r)
      | LFloat l, LInt r -> LBool (l > (Int32.to_float r))
      | LInt l, LFloat r -> LBool ((Int32.to_float l) > r)
      | LFloat l, LFloat r -> LBool (l > r)
      (* Impossible because of typecheck*)
      | _ -> assert false
      end

  (* equality operators *)
  | OpEqual ->
      (* deal with special cases *)
      begin match lhs, rhs with
        (* numbers *)
        | LInt l, LInt r -> LBool (l = r)
        | LFloat l, LInt r -> LBool (l = (Int32.to_float r))
        | LInt l, LFloat r -> LBool ((Int32.to_float l) = r)
        | LFloat l, LFloat r -> LBool (l = r)

        (* enums *)
        | LEnum (lt, lv), LEnum (rt, rv) ->
          (* check if types and values are same *)
          let result = ((value lt) = (value rt))
                    && ((value lv) = (value rv))
          (* Return *)
          in LBool result

        (* 2D vectors *)
        | LVec2 (lx, ly), LVec2 (rx, ry) ->
          (* construct optimized expressions *)
          let optlx = optimise_expr (value lx) in
          let optly = optimise_expr (value ly) in
          let optrx = optimise_expr (value rx) in
          let optry = optimise_expr (value ry) in
          (* Check their types *)
          begin match optlx, optrx, optly, optry with
            (* They are all literals *)
            | ELiteral llx, ELiteral lrx, ELiteral lly, ELiteral lry ->
              (* Compute operations comparing components *)
              let compx =  optimise_operation (value llx) OpEqual (value lrx) in
              let compy =  optimise_operation (value lly) OpEqual (value lry) in
              (* Vector is equal if they're all equal *)
              optimise_operation compx OpAnd compy
            (* One of them could not be reduced to a literal. Fail *)
            | _ -> raise Optimisation_failure
          end

        (* 2D vectors *)
        | LVec3 (lx, ly, lz), LVec3 (rx, ry, rz) ->
          (* construct optimized expressions *)
          let optlx = optimise_expr (value lx) in
          let optly = optimise_expr (value ly) in
          let optlz = optimise_expr (value lz) in
          let optrx = optimise_expr (value rx) in
          let optry = optimise_expr (value ry) in
          let optrz = optimise_expr (value rz) in
          (* Check their types *)
          begin match optlx, optrx, optly, optry, optlz, optrz with
            (* They are all literals *)
            | ELiteral llx, ELiteral lrx, ELiteral lly, ELiteral lry, ELiteral llz, ELiteral lrz ->
              (* Compute operations comparing components *)
              let compx =  optimise_operation (value llx) OpEqual (value lrx) in
              let compy =  optimise_operation (value lly) OpEqual (value lry) in
              let compz =  optimise_operation (value llz) OpEqual (value lrz) in
              (* Vector is equal if they're all equal *)
              optimise_operation (optimise_operation compx OpAnd compy) OpAnd compz
            (* One of them could not be reduced to a literal. Fail *)
            | _ -> raise Optimisation_failure
          end

        (* formatted string *)
        | LString lfstr, LString rfstr->
          (* optimize the fstrings *)
          let optlfstr = optimise_fstring lfstr in
          let optrfstr = optimise_fstring rfstr in
          (* Crawl inside *)
          let rec compare_tok_to_tok str1 str2 = match str1, str2 with
            (* Both empty. They're equal *)
            | [], [] -> true
            (* Not the same length. Not equal *)
            | [], _ -> false
            | _, [] -> false
            (* Compare tokens *)
            | tok1::str1, tok2::str2 -> begin match (value tok1), (value tok2) with
              (* If we have two string tokens, some more can be said *)
              | StrInline e1, StrInline e2 ->
                (* optimize *)
                let opte1 = optimise_expr (value e1) in
                let opte2 = optimise_expr (value e2) in
                (* Check their types *)
                begin match opte1, opte2 with
                  (* Literals. Equal if they're equal *)
                  | ELiteral { value=lit1; _ }, ELiteral { value=lit2; _ } ->
                    (* Compare *)
                    let comparelit = optimise_operation lit1 OpEqual lit2 in
                    (* continue *)
                    (comparelit = (LBool true)) && (compare_tok_to_tok str1 str2)
                  (* Variables. Equal if they are the same variable *)
                  | EVar { value=var1; _ }, EVar { value=var2; _ } ->
                    (* Compare and continue *)
                    (var1 = var2) && (compare_tok_to_tok str1 str2)
                  (* Can't say more. Fail*)
                  | _ -> raise Optimisation_failure
              end
              (* If one of them in an inline expr, can't optimize. Fail *)
              | StrInline _, _ | _, StrInline _ -> raise Optimisation_failure
              (* Else, compare tokens *)
              | _ -> (tok1 = tok2) && (compare_tok_to_tok str1 str2)
            end
          (* start comparison token to token *)
          in LBool (compare_tok_to_tok optlfstr optrfstr)

        (* In other cases *)
        | _ -> LBool (op <> OpEqual)
      end

  (* no eq operator*)
  | OpNotEqual ->
    (* check types *)
    begin match lhs, rhs with
      (* 2D vectors *)
      | LVec2 (lx, ly), LVec2 (rx, ry) ->
        (* construct optimized expressions *)
        let optlx = optimise_expr (value lx) in
        let optly = optimise_expr (value ly) in
        let optrx = optimise_expr (value rx) in
        let optry = optimise_expr (value ry) in
        (* Check their types *)
        begin match optlx, optrx, optly, optry with
          (* The x coord are both literals and they're different *)
          | ELiteral llx, ELiteral lrx, _, _
            when optimise_operation (value llx) OpEqual (value lrx) = LBool false  ->
              (* One coordinate has been defined different. The vectors are not equal *)
              LBool true
          (* The y coord are both literals and they're different *)
          | _, _, ELiteral lly, ELiteral lry
            when optimise_operation (value lly) OpEqual (value lry) = LBool false  ->
              (* One coordinate has been defined different. The vectors are not equal *)
              LBool true
          (* One of them could not be reduced to a literal. Fail *)
          | _ -> raise Optimisation_failure
        end

      (* 2D vectors *)
      | LVec3 (lx, ly, lz), LVec3 (rx, ry, rz) ->
        (* construct optimized expressions *)
        let optlx = optimise_expr (value lx) in
        let optly = optimise_expr (value ly) in
        let optlz = optimise_expr (value lz) in
        let optrx = optimise_expr (value rx) in
        let optry = optimise_expr (value ry) in
        let optrz = optimise_expr (value rz) in
        (* Check their types *)
        begin match optlx, optrx, optly, optry, optlz, optrz with
          (* The x coord are both literals and they're different *)
          | ELiteral llx, ELiteral lrx, _, _, _, _
            when optimise_operation (value llx) OpEqual (value lrx) = LBool false  ->
              (* One coordinate has been determined different. The vectors are not equal *)
              LBool true
          (* The y coord are both literals and they're different *)
          | _, _, ELiteral lly, ELiteral lry, _, _
            when optimise_operation (value lly) OpEqual (value lry) = LBool false  ->
              (* One coordinate has been determined different. The vectors are not equal *)
              LBool true
          (* The z coord are both literals and they're different *)
          | _, _, _, _, ELiteral llz, ELiteral lrz
            when optimise_operation (value llz) OpEqual (value lrz) = LBool false  ->
              (* One coordinate has been determined different. The vectors are not equal *)
              LBool true
          (* One of them could not be reduced to a literal. Fail *)
          | _ -> raise Optimisation_failure
        end

      (* formatted string *)
      | LString lfstr, LString rfstr->
        (* optimize the fstrings *)
        let optlfstr = optimise_fstring lfstr in
        let optrfstr = optimise_fstring rfstr in
        (* Crawl inside *)
        let rec compare_tok_to_tok str1 str2 = match str1, str2 with
          (* Both empty. They're equal *)
          | [], [] -> false
          (* Not the same length. Not equal *)
          | [], _ -> true
          | _, [] -> true
          (* Compare tokens *)
          | tok1::str1, tok2::str2 -> begin match (value tok1), (value tok2) with
            (* If we have two string tokens, some more can be said *)
            | StrInline e1, StrInline e2 ->
              (* optimize *)
              let opte1 = optimise_expr (value e1) in
              let opte2 = optimise_expr (value e2) in
              (* Check their types *)
              begin match opte1, opte2 with
                (* Literals. Different if they're different *)
                | ELiteral { value=lit1; _ }, ELiteral { value=lit2; _ } ->
                  (* Compare *)
                  let comparelit = optimise_operation lit1 OpNotEqual lit2 in
                  (* continue *)
                  (comparelit = (LBool true)) || (compare_tok_to_tok str1 str2)
                (* Variables. If they're equal, this token is no different *)
                | EVar { value=var1; _ }, EVar { value=var2; _ } ->
                  (*
                      -> if the variables are equal, we ignore this token in our difference comparison
                          since they have the same value, hence imply the same substring in the end
                      -> if they're not equal, can't optimize, because they could be different and
                          value equal or different and not value equal
                   *)
                  if var1 = var2 then compare_tok_to_tok str1 str2
                  else raise Optimisation_failure
                (* Can't say more. Fail*)
                | _ -> raise Optimisation_failure
            end
            (* If one of them in an inline expr, can't optimize. Fail *)
            | StrInline _, _ | _, StrInline _ -> raise Optimisation_failure
            (* Else, compare tokens *)
            | _ -> (tok1 <> tok2) || (compare_tok_to_tok str1 str2)
          end
        (* start comparison token to token *)
        in LBool (compare_tok_to_tok optlfstr optrfstr)

      (* No special case. Use equality to simplify *)
      | _ ->
        (* try compute equality *)
        let equal = optimise_operation lhs OpEqual rhs in
        (* Return the opposite *)
        begin match equal with
          (* Must be a boolean *)
          | LBool eq -> LBool (not eq)
          (* If we're here, equal returned something other than a boolean. This is not feasible *)
          | _ -> assert false
        end
    end

(* Optimizes a formatted string *)
and optimise_fstring =
  (* Optimize a token *)
  let optimise_token tok = match tok with
    (* An expression ; try optimize *)
    | StrInline expr -> StrInline (unknown_pos (optimise_expr (value expr)))
    (* Ignore the rest *)
    | _ -> tok
  in function
    (* Empty fstring *)
    | [] -> []
    (* Just one token. Optimize and return *)
    | [tok] -> [unknown_pos (optimise_token (value tok))]
    (* Crawl thru tokens *)
    | tokf::toks::tail ->
      (* Check their type *)
      begin match (value tokf), (value toks) with
        (* Two consts *)
        | StrConst strf, StrConst strs ->
          (* Merge them *)
          unknown_pos (StrConst (strf ^ strs))::(optimise_fstring tail)
        (* Other case. Optimize first token and continue *)
        | _ -> (unknown_pos (optimise_token (value tokf)))::(optimise_fstring (toks::tail))
      end


(* Optimizes an instruction *)
and optimise_instr instr = match instr with
  (* A NOP : it's removed*)
  | INop -> []

  (* A set / ifnset instruction *)
  | IIfnset (var, expr) ->
    (* relocate and set*)
    [ IIfnset (var, unknown_pos (optimise_expr (value expr))) ]
  (* A set / ifnset instruction *)
  | ISet (var, expr) ->
    (* relocate and set*)
    [ ISet (var, unknown_pos (optimise_expr (value expr))) ]

  (* A speed command *)
  | ISpeed expr ->
    (* relocate and set*)
    [ ISpeed (unknown_pos (optimise_expr (value expr))) ]

  (* A wait instruction *)
  | IWait (id, expr) ->
    (* Optimize expression *)
    let optexpr = optimise_expr (value expr) in
    (* Check if there is an id *)
    begin match (id) with
      (* Wait for no event.*)
      | None ->
        (* Check if expr is null *)
        begin match optexpr with
          (* Null. Remove instruction since it does nothing *)
          | ELiteral { value=LFloat 0.; _ }
          | ELiteral { value=LInt 0l; _ } -> []
          (* Not null, don't optimize *)
          | _ -> [IWait (id, unknown_pos optexpr)]
        end
      (* Id is not null ; can't optimize *)
      | Some _ -> [IWait (id, unknown_pos optexpr)]
    end

  (* A message. Optimize the fstring *)
  | IMessage (msg) ->
    (* destruct *)
    let fstr, ops = value msg in
    (* reconstruct *)
    [IMessage (unknown_pos (optimise_fstring fstr, ops))]

  (* A choice *)
  | IChoice choices ->
    (* Optimise a choice *)
    let optimise_choice choice =
      (* deconstruct *)
      let msg, prog = choice in
      let fstr, opts = value msg in
      (* opimise fstring and program *)
      let optfstr = optimise_fstring fstr in
      let optprog = optimise_program (value prog) in
      (* return the choice optimised *)
      (unknown_pos (optfstr, opts), unknown_pos optprog)
    in
    (* Optimize each message and program *)
    [IChoice (List.map optimise_choice choices)]

  (* A condition *)
  | ICondition (matched, branches) ->
    (* optimise matched *)
    let optmatched = optimise_expr (value matched) in
    (* optimize branches *)
    let rec optimise_branches isfirstbranch branches = begin match branches with
      | [] -> []
      | (patt, prog)::tail ->
        (* check patterns *)
        let optimise_pattern = function
          (* A wildcard *)
          | PWildcard ->
            (* if we're in the first branch, we always use this branch *)
            if isfirstbranch
            then raise (Optimisation_always_used_branch (value prog));
            (* return pattern *)
            raise (Optimisation_unused_branches_after PWildcard)
          (* A simple value *)
          | PValue expr ->
            (* optimise expression *)
            Some (PValue (unknown_pos (optimise_expr (value expr))))
          (* A binding expression *)
          | PBinding (id, expr) ->
            (* optimize expression *)
            let optexpr = optimise_expr (value expr) in
            (* check its type *)
            begin match optexpr with
              (* the expression is a literal, and it's trye *)
              | ELiteral { value= LBool true; _ } ->
                (* it's always reached. *)
                (* if we're the first branch, we always use this branch *)
                if isfirstbranch
                then raise (Optimisation_always_used_branch (value prog));
                (* we're not the first branch. cut short *)
                raise (Optimisation_unused_branches_after (PBinding (id, unknown_pos optexpr)))
              (* the expression is a literal, and it's false *)
              | ELiteral { value= LBool false; _ } ->
                (* Unreachable *)
                None
              (* anything else *)
              | _ -> Some (PBinding (id, unknown_pos optexpr))


            end
        in
        (* optimise the pattern *)
        match optimise_pattern (value patt) with
          (* A wildcard pattern has been matched. End *)
          | exception Optimisation_unused_branches_after patt ->
            (* optimise the program *)
            let optprog = optimise_program (value prog) in
            (* return the optimised pattern *)
            [(unknown_pos patt, unknown_pos optprog)]
          (* Unreachable *)
          | None -> (optimise_branches isfirstbranch tail)
          | Some patt ->
            (* optimise the program *)
            let optprog = optimise_program (value prog) in
            (* return the optimised pattern *)
            (unknown_pos patt, unknown_pos optprog)::(optimise_branches false tail)
    end in
    (* start optimizing branches*)
    begin try begin
      (* Optimize branches *)
      let optbranches = optimise_branches true branches in
      (* Return the condition *)
      [ ICondition (unknown_pos optmatched, optbranches) ]
    end with
      (* Cut short ; this branch is always used, optimise *)
      | Optimisation_always_used_branch prog -> List.rev (List.rev_map value prog)
    end

  (* Don't optimise *)
  | _ -> [instr]

(* Optimizes a program *)
and optimise_program program =
  let rec optimise_subprogram program = match program with
    (* An empty program. Return it *)
    | [] -> []
    (* An instruction. Optimize it. If it is still there, add it, else remove it  *)
    | instr::tail -> begin match optimise_instr (value instr) with
        (* Instruction has been deleted *)
        | [] -> (optimise_subprogram tail)
        (* Instruction has been replaced by one instruction *)
        | [i] -> (unknown_pos i)::(optimise_subprogram tail)
        (* Instruction has been replaced by more instructions *)
        | instrs -> List.rev_append (List.rev_map unknown_pos instrs) (optimise_subprogram tail)
      end
  in
  (* optimize subprogram *)
  let optprog = optimise_subprogram program in
  (* check if empty. If it is, return a nop *)
  if optprog = [] then [ unknown_pos INop ] else optprog
