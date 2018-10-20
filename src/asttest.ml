open Lexing
open DLG.Ast
open Printf
open Utils.Position

let usage () = eprintf "usage: dlg file\n"; exit 1

let indent_ctr = ref 0
let rec make_indent n = match n with
  | 0 -> ""
  | _ -> "  " ^ (make_indent (n-1))
let string_of_indent () = make_indent (!indent_ctr)
let indent () = indent_ctr := ((!indent_ctr) + 1)
let deindent () = indent_ctr := ((!indent_ctr) - 1)
let indent_block str = begin
  let () = indent ()
  in let center = str ^ "\n"
  in let () = deindent()
  in center end

let rec string_of_program program = match program with
  | [] -> "\n"
  | [i] -> (string_of_indent ()) ^ (string_of_instruction (value i))
  | i::tail -> (string_of_indent ()) ^ (string_of_instruction (value i)) ^ "\n" ^ (string_of_program tail)

and string_of_instruction instr = match instr with
  | ILabel id -> "label " ^ (string_of_id (value id))
  | IGoto id -> "goto " ^ (string_of_id (value id))
  | ISet (var, expr) -> "set " ^ (string_of_variable (value var)) ^ " " ^ (string_of_expr (value expr))
  | IIfnset (var, expr) -> "ifnset " ^ (string_of_variable (value var)) ^ " " ^ (string_of_expr (value expr))
  | IWait (evt, expr) -> let event = begin match evt with
      | Some ev -> (string_of_id (value ev))
      | None -> "_"
    end in
    "wait " ^ event ^ " " ^ (string_of_expr (value expr))
  | INop -> "nop"
  | IMessage msg -> (string_of_message (value msg))
  | IChoice choices ->
      let rec string_of_choices = (function
        | [] -> "\n"
        | (msg, prog)::t -> let header = string_of_indent() ^ "-" ^ string_of_message (value msg) ^  "\n"
                            in let prefix = (let () = indent() in string_of_program (value prog))
                            in let () = deindent()
                            in let rest = "\n" ^ (string_of_choices t)
                            in header ^ prefix ^ rest
        )
      in let prefix = "?\n" ^ (let () = indent() in (string_of_choices choices))
      in prefix ^ (let () = deindent() in string_of_indent())
  | ICondition (ex, conditions) ->
      let rec string_of_conditions = (function
        | [] -> "\n"
        | (pat, prog)::t -> let header = string_of_indent() ^ "-" ^ string_of_pattern (value pat) ^  "\n"
                            in let prefix = (let () = indent() in string_of_program (value prog))
                            in let () = deindent()
                            in let rest = "\n" ^ (string_of_conditions t)
                            in header ^ prefix ^ rest
        )
      in let prefix = "?" ^ (string_of_expr (value ex)) ^ "\n" ^ (let () = indent() in (string_of_conditions conditions))
      in prefix ^ (let () = deindent() in string_of_indent())
  | IInvoke (func, args) ->
    let rec print_args l = match l with
      | [] -> ""
      | arg::[] -> (string_of_expr (value arg))
      | arg::tail -> (string_of_expr (value arg)) ^ ", " ^ (print_args tail)
    in "invoke " ^ (string_of_id (value func)) ^ "(" ^ (print_args (value args)) ^ ")"
  | ISpeed ex -> "speed " ^ (string_of_expr (value ex))
  | ISend (id, arg) ->
    let argstr = match arg with
      | None -> "_"
      | Some x -> string_of_expr (value x)
    in "send " ^ (string_of_id (value id)) ^ " " ^ argstr
  | IDeclare (var, t, args) ->
    let varstr = string_of_variable (value var) in
    let tstr = string_of_type (value t) in
    let argsstr = begin match args with
      | None -> ""
      | Some a -> string_of_typelist a
    end in
    "declare " ^ varstr ^ " " ^ tstr ^ " " ^ argsstr

and string_of_type t = match t with
  | TInt -> "int"
  | TFloat -> "float"
  | TBool -> "bool"
  | TString -> "string"
  | TEnum s -> "enum(" ^ s ^ ")"
  | TVec2 -> "vec2"
  | TVec3 -> "vec3"
  | TUnknown -> "'t"
  | TVoid -> "void"

and string_of_typelist tl =
  let rec string_of_typelist_args = function
    | [] -> ""
    | t::h -> (string_of_type t) ^ "; " ^ (string_of_typelist_args h)
  in "(" ^ (string_of_typelist_args (value tl)) ^ ")"

and string_of_pattern p = match p with
  | PWildcard -> "_"
  | PValue expr -> string_of_expr (value expr)
  | PBinding (id, ex) -> (string_of_id (value id)) ^ " when " ^ (string_of_expr (value ex))

and string_of_expr expr =
  let rec print_args l = match l with
    | [] -> ""
    | arg::[] -> (string_of_expr (value arg))
    | arg::tail -> (string_of_expr (value arg)) ^ ", " ^ (print_args tail)
  in match expr with
    (* A literal (const value) *)
    | ELiteral lit -> string_of_literal (value lit)
    (* A variable *)
    | EVar var -> string_of_variable (value var)
    (* An operation *)
    | EOperation (str, lhs, rhs) -> (string_of_operation (value str)) ^ "(" ^ (string_of_expr (value lhs)) ^ ", " ^ (string_of_expr (value rhs)) ^ ")"
    (* A function call *)
    | EFunc (id, args) -> (string_of_id (value id)) ^ ":(" ^ (print_args (value args)) ^ ")"
    (* A condition *)
    | ECondition (cond, a, b) -> "if(" ^ (string_of_expr (value cond)) ^ "){" ^ (string_of_expr (value a)) ^ "}" ^ "{" ^ (string_of_expr (value b)) ^ "}"
    (* Access to a type *)
    | EAccess(vec, id) ->  (string_of_expr (value vec)) ^ ".[" ^ (string_of_id (value id)) ^ "]"
    (*Value cast *)
    | ETypeCast (dest, expr) -> "(" ^ (string_of_type (value dest)) ^ ")(" ^  (string_of_expr (value expr)) ^ ")"

and string_of_variable var =
  let sc, id = var in
  "{" ^ (string_of_scope sc) ^ ":" ^ (string_of_id id) ^ "}"

and string_of_scope scope = match scope with
  | SGlobal -> "global"
  | SLocal -> "local"
  | SExtern -> "extern"

and string_of_message msg =
  let rec string_of_messageopts = function
    | [] -> ""
    | h::t -> (string_of_messageopt (value h)) ^ " " ^ (string_of_messageopts t)
  in match msg with
    | str, opts -> "\"" ^ (string_of_fstring str) ^ "\" " ^ (string_of_messageopts opts)

and string_of_messageopt = function
  | MsgNoRush -> "norush"
  | MsgNoAcknowledge -> "noack"

and string_of_literal = function
  | LInt i -> Int32.to_string i
  | LFloat f -> string_of_float f
  | LBool b -> string_of_bool b
  | LString fs -> "'" ^ (string_of_fstring fs) ^ "'"
  | LEnum (t, v) -> "enum[" ^ (string_of_id (value t)) ^ "." ^ (string_of_id (value v)) ^ "]"
  | LVec2 (x, y) -> "vec2[" ^ (string_of_expr (value x)) ^  " ; " ^ (string_of_expr (value y)) ^ "]"
  | LVec3 (x, y, z) -> "vec3[" ^ (string_of_expr (value x)) ^  " ; " ^ (string_of_expr (value y)) ^ " ; " ^ (string_of_expr (value z)) ^ "]"

and string_of_fstring = function
  | [] -> ""
  | tok::tail -> (string_of_fstring_tok (value tok)) ^ (string_of_fstring tail)

and string_of_fstring_tok tok = let out = match tok with
   | StrConst s -> String.escaped s
   | StrInline e -> (string_of_expr (value e))
   | StrColor col -> begin match col with
     | None -> "[#{}]"
     | Some s -> "[#{" ^ s ^ "}]"
     end
    in "["^out^"]"

and string_of_operation = function
  | OpPlus -> "+"
  | OpMinus -> "-"
  | OpStar -> "*"
  | OpDivide -> "/"
  | OpAnd -> "&&"
  | OpOr -> "||"
  | OpEqual -> "=="
  | OpNotEqual -> "!="
  | OpLeq -> "<="
  | OpGeq -> ">="
  | OpLess -> "<"
  | OpMore -> ">"

and string_of_id id = match id with
  | Id s -> s

let main () =
  if Array.length Sys.argv <> 2 then usage ();
  let c = open_in Sys.argv.(1) in
  let lb = from_channel c in
  let ast = try DLG.Parser.program (DLG.Lexer.main false) lb
            with
              |  Parsing.Parse_error -> Utils.Error.print_syntax_error_at c lb; exit 0
            in
  let str = (string_of_program ast) ^ "\n" in
  print_string str;

  close_in c

let _ = Printexc.catch main ()
