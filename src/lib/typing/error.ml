open DLG.Ast
open Lexing
open Utils.Position

(* A variant containing the data  *)
type type_error_reason =
  | ReasonUnbound
  | ReasonIncompatibleDeclaration
  | ReasonNotANumber
  | ReasonNotAValue
  | ReasonNotAFunction
  | ReasonConditionWrongType
  | ReasonBranchesDifferentType
  | ReasonNoProperty
  | ReasonInvalidTypeCast
  | ReasonInvalidTypeOp
  | ReasonInvalidValuePattern
  | ReasonInvalidBindingPattern
  | ReasonAlreadyBound
  | ReasonInvalidCall
  | ReasonNotVoid
  | ReasonNoProduceVoid


(* A payload for a type error exception*)
type type_error = {
  position: position option; (* the position in the lexbuf of the token *)
  reason: type_error_reason;          (* the reason for the error *)
  expected: type_container list;
  given: type_container option;
}

(* An exception raised whenever there's a typing error *)
exception Type_error of type_error

(* gets the reason of a type error *)
let reason { reason = r; _ } = r
(* gets the given type of a type error *)
let given { given = g; _ } = g
(* gets the expected type of a type error *)
let expected { expected = e; _ } = e

(* makes a type error *)
let make_type_error reason given expected = {
  position = None;
  reason = reason;
  expected = expected;
  given = given; }
(* raise a type error w/ location data *)
let make_located_type_error reason token given expected =
  let payload = { position = Some (position token); reason = reason; expected = expected; given = given; } in
  (Type_error payload)
(* raise a type error *)
let raise_type_error reason given expected =
  let payload = { position = None; reason = reason; expected = expected; given = given; } in
  raise (Type_error payload)
(* raise a type error w/ location data *)
let raise_located_type_error reason token given expected =
  let payload = { position = Some (position token); reason = reason; expected = expected; given = given; } in
  raise (Type_error payload)
(* get a result, checks if it is okay, else locate the exception and throw  *)
let check_for_type_error res tok = match res with
  (* no error. return *)
  | Ok x -> x
  (* error. locate and throw *)
  | Error payload -> raise_located_type_error (reason payload) tok (given payload) (expected payload)

(* Prints a type error *)
let print_type_error_at err file =
  (* prints a typecontainer type *)
  let string_of_type_container =
    (* the string for a given type *)
    let string_of_type t = match t with
      | TInt -> "int"
      | TFloat -> "float"
      | TBool -> "bool"
      | TString -> "string"
      | TEnum s -> "enum(" ^ s ^ ")"
      | TVec2 -> "vec2"
      | TVec3 -> "vec3"
      | TUnknown -> "'t"
      | TVoid -> "void"
    (* the string for a function type *)
    in let string_of_type_func tf =
      (* get the return and argument types *)
      let ret, args = tf in
      (* print an argument type list *)
      let rec string_of_type_func_args = function
        | [] -> string_of_type TVoid
        | [t0] -> string_of_type t0
        | t0::tail -> (string_of_type t0) ^ " * " ^ (string_of_type_func_args tail)
      in
      (* print the function type *)
      (string_of_type ret) ^ " <- " ^ (string_of_type_func_args args)
    in function
      | TCValue t -> string_of_type t
      | TCFunc f -> string_of_type_func f
  in
  (* the reason line *)
  let reason = match err.reason with
    | ReasonUnbound -> "Unbound identifier"
    | ReasonIncompatibleDeclaration -> "Identifier was already declared with a different signature"
    | ReasonNotANumber -> "Expression is not a number"
    | ReasonNotAValue -> "Identifier is not a value"
    | ReasonNotAFunction -> "Identifier is not a function"
    | ReasonConditionWrongType -> "Condition must be a boolean"
    | ReasonBranchesDifferentType -> "Branches of a ternary condition must have the same type"
    | ReasonNoProperty -> "No property of type matching given name"
    | ReasonInvalidTypeCast -> "Could not cast to given type"
    | ReasonInvalidTypeOp -> "Operator does not work with given types"
    | ReasonInvalidValuePattern -> "A value pattern in a condition branch must have the same type as the matched value"
    | ReasonInvalidBindingPattern -> "A binding pattern condition must be a boolean"
    | ReasonAlreadyBound -> "Identifier is already bound"
    | ReasonInvalidCall -> "Argument list provided to the function does not match its signature"
    | ReasonNotVoid -> "Function must return void"
    | ReasonNoProduceVoid -> "A value can never be void"
  in
  (* the expected line *)
  let expected = match err.expected with
    | [] -> ""
    | _ ->
      let expectedtypes = String.concat " or " (List.map string_of_type_container err.expected) in
      "    was expected : " ^ expectedtypes ^ "\n"
  in
  (* the expected line *)
  let given = match err.given with
    | None -> ""
    | Some tc -> "    was given : " ^ (string_of_type_container tc) ^ "\n"
  in
  (* cut the code line *)
  let string_of_file_line line start e =
    (* nothing. return full line *)
    if start == e then (line, "", "")
    (* cut *)
    else begin
      let bef = (String.sub line 0 start) in
      let cur = (String.sub line start (e-start)) in
      let aft = (String.sub line e ((String.length line) - e)) in
      (bef, cur, aft)
    end
  in
  (* get the code line from file *)
  let get_line file p =
    let pos_start_line = p.pos_bol in
      (* set at beginning of line *)
      seek_in file pos_start_line;
      (* read line *)
      input_line file
  in
  (* check if there is a position *)
  (match err.position, file with
    (* A position AND A file. Cut line and print *)
    | Some p, Some f -> begin
      (* get the line *)
      let scriptline = get_line f p.start_p in
      (* get the postions in the line *)
      let startpos = (column p.start_p) in
      let endpos = (column p.end_p) in
      let linepos = (line p.start_p) in
      (* Cut line *)
      let (bef, cur, aft) = string_of_file_line scriptline startpos endpos in
      (* Print error *)
      ANSITerminal.print_string [] ("`" ^ bef);
      ANSITerminal.print_string [ANSITerminal.red; ANSITerminal.Underlined] cur;
      ANSITerminal.print_string [] aft;
      ANSITerminal.print_string [] "`\n";
      ANSITerminal.print_string [] ("Line " ^ (string_of_int linepos));
      ANSITerminal.print_string [] (" characters " ^ (string_of_int startpos));
      ANSITerminal.print_string [] (":" ^ (string_of_int endpos) ^ "\n");
    end
    (* No position or file. Print nothing *)
    | _ -> ());
  (* print the reason *)
  ANSITerminal.print_string [] "Type error : ";
  ANSITerminal.print_string [] (reason ^ "\n");
  (* the message *)
  ANSITerminal.print_string [] expected;
  ANSITerminal.print_string [] given
