%{
  open DlgAST
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
%token KEYWORD_NOP
(* Literals *)
%token<bool> LITERAL_BOOL
%token<int32> LITERAL_INT
%token<float> LITERAL_FLOAT
(* Identifiers *)
%token<string> ID_VAR

(* Operators *)
%token OPERATOR_CHOICE
%token OPERATOR_CHOICEOPTION
%token<string> OPERATOR_MESSAGE

(* Punctuation *)
%token PUNCTUATION_PERCENT

%start<DlgAST.t> program


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
    { ISet (sc, var, e) }
  | KEYWORD_IFNSET sc=located(scope) var=located(identifier_var) e=located(expr)
    { IIfnset (sc, var, e) }
  | KEYWORD_WAIT n=located(number)
    { IWait n }
  | KEYWORD_NOP
    { INop }
  | OPERATOR_MESSAGE (*TODO : change *)
    { IMessage "" }
  (* multiline *)
  | OPERATOR_CHOICE INDENT options=list(choiceoption) OUTDENT
    { IChoice(options) }

choiceoption:
  | OPERATOR_CHOICEOPTION str=located(OPERATOR_MESSAGE) INDENT s=located(subprogram) OUTDENT
    { (str, s) }

(* Expressions *)
expr:
  | lit = located(literal)
    { ELiteral(lit) }

(* Keywords *)
scope:
  | KEYWORD_GLOBAL
    { SGlobal }
  | KEYWORD_LOCAL
    { SLocal }

(* Identifiers *)
identifier_var:
  | id = ID_VAR
    { Id id }

(* Literals *)
literal:
  | b = LITERAL_BOOL { LBool b }
  | n = number       { n }

number:
  | i = LITERAL_INT { LInt i }
  | f = LITERAL_FLOAT { LFloat f }
  | p = LITERAL_INT PUNCTUATION_PERCENT { LFloat ((Int32.to_float p) /. 100.) }

(* Inline functions & macros *)
%inline located(X): x=X {
  Position.with_poss $startpos $endpos x
}
