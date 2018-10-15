open Lexing
open DlgParser
open DlgAST
open Printf

let usage () = eprintf "usage: dlg file\n"; exit 1

let string_of_operation = function
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

let print_tok = function
  | EOF -> "EOF"
  | STRING_INLINE -> "STRING_INLINE"
  | STRING_CONST s -> "STRING_CONST(" ^ s ^ ")"
  | STRING_COLOR c -> begin match c with
    | None -> "STRING_COLOR(None)"
    | Some hash -> "STRING_COLOR(" ^ hash ^ ")"
    end
  | PUNCTUATION_RPAREN -> ")"
  | PUNCTUATION_PERCENT -> "%"
  | PUNCTUATION_LPAREN -> "("
  | PUNCTUATION_COMMA -> ","
  | OUTDENT -> "OUTDENT"
  | OPERATOR_WILDCARD -> "_"
  | OPERATOR_STRING -> "\'"
  | OPERATOR_MESSAGE -> "\""
  (* | OPERATOR_CHOICEOPTION -> "-" *)
  | OPERATOR_CHOICE -> "?"
  | OPERATION_STAR s -> string_of_operation s
  | OPERATION_PLUS s -> string_of_operation s
  | OPERATION_OR s -> string_of_operation s
  | OPERATION_MORE s -> string_of_operation s
  | OPERATION_MINUS s -> string_of_operation s
  | OPERATION_LESS s -> string_of_operation s
  | OPERATION_LEQ s -> string_of_operation s
  | OPERATION_ISEQ s -> string_of_operation s
  | OPERATION_ISNEQ s -> string_of_operation s
  | OPERATION_GEQ s -> string_of_operation s
  | OPERATOR_OPTION -> "->"
  | OPERATION_DIVIDE s -> string_of_operation s
  | OPERATION_AND s -> string_of_operation s
  | LITERAL_INT i -> Int32.to_string i
  | LITERAL_FLOAT f -> string_of_float f
  | LITERAL_BOOL b -> string_of_bool b
  | LITERAL_ENUM -> "enum"
  | LITERAL_VEC2 -> "vec2"
  | LITERAL_VEC3 -> "vec3"
  | KEYWORD_WHEN -> "when"
  | KEYWORD_WAIT -> "wait"
  | KEYWORD_SET -> "set"
  | KEYWORD_NORUSH -> "norush"
  | KEYWORD_INVOKE -> "invoke"
  | KEYWORD_SEND -> "send"
  | KEYWORD_EXTERN -> "SCOPE(extern)"
  | KEYWORD_NOP -> "nop"
  | KEYWORD_NOACK -> "noack"
  | KEYWORD_LOCAL -> "SCOPE(local)"
  | KEYWORD_LABEL -> "label"
  | KEYWORD_IFNSET -> "ifnset"
  | KEYWORD_THEN -> "then"
  | KEYWORD_SPEED -> "speed"
  | KEYWORD_GOTO -> "goto"
  | KEYWORD_GLOBAL -> "SCOPE(global)"
  | INDENT -> "INDENT"
  | ID_VAR s -> "VAR(" ^ s ^ ")"
  | ID_OBJECT s -> "OBJ(" ^ s ^ ")"
  | PUNCTUATION_HASH -> "#"
  | OPERATOR_TERNARY -> "?="
  | PUNCTUATION_COLON -> ":"

let main () =
  if Array.length Sys.argv <> 2 then usage ();
  let c = open_in Sys.argv.(1) in
  let lb = from_channel c in
  (* let p = Parser.program Lexer.main lb in *)
  let rec tokenize () =
    let tok = DlgLexer.main false lb in
    print_string (print_tok tok); print_string "\n";
    if tok <> EOF then tokenize () else ()
  in tokenize ();
  close_in c

let _ = Printexc.catch main ()
