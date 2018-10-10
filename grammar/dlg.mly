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
%token OPERATOR_MESSAGE

(* Message *)
%token STRING_INLINE
%token<string> STRING_CONST
%token<string option> STRING_COLOR

(* Punctuation *)
%token PUNCTUATION_PERCENT
%token PUNCTUATION_LPAREN
%token PUNCTUATION_RPAREN

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
  | msg = located(message) (*TODO : change *)
    { IMessage msg }
  (* multiline *)
  | OPERATOR_CHOICE INDENT options=list(choiceoption) OUTDENT
    { IChoice(options) }

choiceoption:
  | OPERATOR_CHOICEOPTION str=located(message) INDENT s=located(subprogram) OUTDENT
    { (str, s) }

(* Expressions *)
expr:
  | lit = located(literal)
    { ELiteral(lit) }
  | var=located(identifier_var)
    { EVar(var) }

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

(* String tokens*)
message:
  | OPERATOR_MESSAGE str=stringcontents OPERATOR_MESSAGE
    { str }

stringcontents:
  | sub = located(substring) tail=stringcontents
    { sub::tail }
  | sub = located(substring)
    { [sub] }

stringinline:
  | STRING_INLINE e=located(expr) STRING_INLINE
    { e }

substring:
  | c = STRING_CONST { StrConst c }
  | e = stringinline { StrInline e }
  | col = STRING_COLOR { StrColor col }

(* Inline functions & macros *)
%inline located(X): x=X {
  Position.with_poss $startpos $endpos x
}
