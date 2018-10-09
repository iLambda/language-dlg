%{
  open DLGAst
  open Position
%}

(*
 * TOKENS
 *)
%token EOF

(* Structure *)
%token INDENT
%token OUTDENT
(* Keywords *)
%token KEYWORD_LABEL
%token KEYWORD_GOTO
%token KEYWORD_SET
%token KEYWORD_IFNSET
%token KEYWORD_GLOBAL
%token KEYWORD_LOCAL
%token KEYWORD_WAIT
(* Literals *)
%token<bool> LITERAL_BOOL
%token<int> LITERAL_INT
%token<float> LITERAL_FLOAT
(* Identifiers *)
%token<string> ID_VAR

(* Operators *)
%token OPERATOR_CHOICE
%token OPERATOR_CHOICEOPTION
%token OPERATOR_MESSAGE

(* Punctuation *)
%token PUNCTUATION_PERCENT

%start<DLGAst.t> program


(*
 * PRIORITY RULES
 *)

%%

(*
 * GRAMMAR RULES
 *)


program: l = subprogram EOF
  { l }

subprogram: l = list(located(instruction))
  { l }

instruction:
  (* one liners *)
  | KEYWORD_LABEL id=located(identifier_var)
    { ILabel id }
  | KEYWORD_GOTO id=located(identifier_var)
    { IGoto id }
  | KEYWORD_SET sc=located(scope) var=located(identifier_var) e=located(expr)
    { ISet (var, sc, e) }
  | KEYWORD_IFNSET sc=located(scope) var=located(identifier_var) e=located(expr)
    { IIfnset (var, sc, e) }
  | KEYWORD_WAIT n=located(number)
    { IWait n }
  | OPERATOR_MESSAGE (*TODO : change *)
    { IMessage "" }
  (* multiline *)
  | OPERATOR_CHOICE INDENT options=list(located(choiceoption)) OUTDENT
    { IChoice(options) }

choiceoption:
  | OPERATOR_CHOICEOPTION INDENT s=subprogram OUTDENT
    { IChoiceOption(s) }

(* Expressions *)
expr:
  | lit = located(literal)
    { ELiteral(lit) }

(* Keywords *)
scope:
  | KEYWORD_GLOBAL
    { SCOPE_GLOBAL }
  | KEYWORD_LOCAL
    { SCOPE_GLOBAL }

(* Identifiers *)
identifier_var:
  | id = ID_VAR
    { Id id }

(* Literals *)
literal:
  | b = LITERAL_BOOL { b }
  | n = number       { n }

number:
  | i = LITERAL_INT { i }
  | f = LITERAL_FLOAT { f }
  | p = LITERAL_INT PUNCTUATION_PERCENT { (float_of_int p) /. 100. }

(* Inline functions & macros *)
%inline located(X): x=X {
  Position.with_poss $startpos $endpos x
}
