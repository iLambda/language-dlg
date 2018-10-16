open Lexing
open DlgParser
open DlgAST
open Printf

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

let unlocate x = Position.value x

let rec string_of_program program = match program with
  | [] -> "\n"
  | [i] -> (string_of_indent ()) ^ (string_of_instruction (unlocate i))
  | i::tail -> (string_of_indent ()) ^ (string_of_instruction (unlocate i)) ^ "\n" ^ (string_of_program tail)

and string_of_instruction instr = match instr with
  | ILabel id -> "label " ^ (string_of_id (unlocate id))
  | IGoto id -> "goto " ^ (string_of_id (unlocate id))
  | ISet (var, expr) -> "set " ^ (string_of_variable (unlocate var)) ^ " " ^ (string_of_expr (unlocate expr))
  | IIfnset (var, expr) -> "ifnset " ^ (string_of_variable (unlocate var)) ^ " " ^ (string_of_expr (unlocate expr))
  | IWait (evt, expr) -> let event = begin match evt with
      | Some ev -> (string_of_id (unlocate ev))
      | None -> "_"
    end in
    "wait " ^ event ^ " " ^ (string_of_expr (unlocate expr))
  | INop -> "nop"
  | IMessage msg -> (string_of_message (unlocate msg))
  | IChoice choices ->
      let rec string_of_choices = (function
        | [] -> "\n"
        | (msg, prog)::t -> let header = string_of_indent() ^ "-" ^ string_of_message (unlocate msg) ^  "\n"
                            in let prefix = (let () = indent() in string_of_program (unlocate prog))
                            in let () = deindent()
                            in let rest = "\n" ^ (string_of_choices t)
                            in header ^ prefix ^ rest
        )
      in let prefix = "?\n" ^ (let () = indent() in (string_of_choices choices))
      in prefix ^ (let () = deindent() in string_of_indent())
  | ICondition (ex, conditions) ->
      let rec string_of_conditions = (function
        | [] -> "\n"
        | (pat, prog)::t -> let header = string_of_indent() ^ "-" ^ string_of_pattern (unlocate pat) ^  "\n"
                            in let prefix = (let () = indent() in string_of_program (unlocate prog))
                            in let () = deindent()
                            in let rest = "\n" ^ (string_of_conditions t)
                            in header ^ prefix ^ rest
        )
      in let prefix = "?" ^ (string_of_expr (unlocate ex)) ^ "\n" ^ (let () = indent() in (string_of_conditions conditions))
      in prefix ^ (let () = deindent() in string_of_indent())
  | IInvoke (func, args) ->
    let rec print_args l = match l with
      | [] -> ""
      | arg::[] -> (string_of_expr (unlocate arg))
      | arg::tail -> (string_of_expr (unlocate arg)) ^ ", " ^ (print_args tail)
    in "invoke " ^ (string_of_id (unlocate func)) ^ "(" ^ (print_args (unlocate args)) ^ ")"
  | ISpeed ex -> "speed " ^ (string_of_expr (unlocate ex))
  | ISend (id, arg) ->
    let argstr = match arg with
      | None -> "_"
      | Some x -> string_of_expr (unlocate x)
    in "send " ^ (string_of_id (unlocate id)) ^ " " ^ argstr
  | IDeclare (var, t, args) ->
    let varstr = string_of_variable (unlocate var) in
    let tstr = string_of_type (unlocate t) in
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
  | TAll -> assert false

and string_of_typelist tl =
  let rec string_of_typelist_args = function
    | [] -> ""
    | t::h -> (string_of_type (unlocate t)) ^ "; " ^ (string_of_typelist_args h)
  in "(" ^ (string_of_typelist_args tl) ^ ")"

and string_of_pattern p = match p with
  | PWildcard -> "_"
  | PValue expr -> string_of_expr (unlocate expr)
  | PBinding (id, ex) -> (string_of_id (unlocate id)) ^ " when " ^ (string_of_expr (unlocate ex))

and string_of_expr expr =
  let rec print_args l = match l with
    | [] -> ""
    | arg::[] -> (string_of_expr (unlocate arg))
    | arg::tail -> (string_of_expr (unlocate arg)) ^ ", " ^ (print_args tail)
  in match expr with
    (** A literal (const value) **)
    | ELiteral lit -> string_of_literal (unlocate lit)
    (** A variable **)
    | EVar var -> string_of_variable (unlocate var)
    (** An operation **)
    | EOperation (str, lhs, rhs) -> (string_of_operation (unlocate str)) ^ "(" ^ (string_of_expr (unlocate lhs)) ^ ", " ^ (string_of_expr (unlocate rhs)) ^ ")"
    (** A function call **)
    | EFunc (id, args) -> (string_of_id (unlocate id)) ^ ":(" ^ (print_args (unlocate args)) ^ ")"
    (** A condition **)
    | ECondition (cond, a, b) -> "if(" ^ (string_of_expr (unlocate cond)) ^ "){" ^ (string_of_expr (unlocate a)) ^ "}" ^ "{" ^ (string_of_expr (unlocate b)) ^ "}"
    (** Access to a type *)
    | EAccess(vec, id) ->  (string_of_expr (unlocate vec)) ^ ".[" ^ (string_of_id (unlocate id)) ^ "]"

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
    | h::t -> (string_of_messageopt (unlocate h)) ^ " " ^ (string_of_messageopts t)
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
  | LEnum (t, v) -> "enum[" ^ (string_of_id (unlocate t)) ^ "." ^ (string_of_id (unlocate v)) ^ "]"
  | LVec2 (x, y) -> "vec2[" ^ (string_of_expr (unlocate x)) ^  " ; " ^ (string_of_expr (unlocate y)) ^ "]"
  | LVec3 (x, y, z) -> "vec3[" ^ (string_of_expr (unlocate x)) ^  " ; " ^ (string_of_expr (unlocate y)) ^ " ; " ^ (string_of_expr (unlocate z)) ^ "]"

and string_of_fstring = function
  | [] -> ""
  | tok::tail -> (string_of_fstring_tok (unlocate tok)) ^ (string_of_fstring tail)

and string_of_fstring_tok tok = let out = match tok with
   | StrConst s -> String.escaped s
   | StrInline e -> (string_of_expr (unlocate e))
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
  let ast = try DlgParser.program (DlgLexer.main false) lb
            with
              |  Parsing.Parse_error -> Error.print_syntax_error_at c lb; exit 0
            in
  let str = (string_of_program ast) ^ "\n" in
  print_string str;

  close_in c

let _ = Printexc.catch main ()
