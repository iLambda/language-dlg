open Lexing
open DlgParser
open Printf

let usage () = eprintf "usage: dlg file\n"; exit 1

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
  | OUTDENT -> "OUTDENT"
  | OPERATOR_WILDCARD -> "_"
  | OPERATOR_STRING -> "\'"
  | OPERATOR_MESSAGE -> "\""
  | OPERATOR_CHOICEOPTION -> "-"
  | OPERATOR_CHOICE -> "?"
  | OPERATION_STAR s -> s
  | OPERATION_PLUS s -> s
  | OPERATION_OR s -> s
  | OPERATION_MORE s -> s
  | OPERATION_MINUS s -> s
  | OPERATION_LESS s -> s
  | OPERATION_LEQ s -> s
  | OPERATION_ISEQ s -> s
  | OPERATION_ISNEQ s -> s
  | OPERATION_GEQ s -> s
  | OPERATION_DIVIDE s -> s
  | OPERATION_AND s -> s
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
  | KEYWORD_OBJECT -> "SCOPE(object)"
  | KEYWORD_NOP -> "nop"
  | KEYWORD_NOACK -> "noack"
  | KEYWORD_LOCAL -> "SCOPE(local)"
  | KEYWORD_LABEL -> "label"
  | KEYWORD_IFNSET -> "ifnset"
  | KEYWORD_SPEED -> "speed"
  | KEYWORD_GOTO -> "goto"
  | KEYWORD_GLOBAL -> "SCOPE(global)"
  | INDENT -> "INDENT"
  | ID_VAR s -> "VAR(" ^ s ^ ")"
  | ID_OBJECT s -> "OBJ(" ^ s ^ ")"


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
