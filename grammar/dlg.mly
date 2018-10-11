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
%token KEYWORD_WHEN
%token KEYWORD_NORUSH
%token KEYWORD_NOACK

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
%token OPERATOR_WILDCARD
%token OPERATOR_STRING

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
  (** a label **)
  | KEYWORD_LABEL id=located(identifier_var)
    { ILabel id }
  (** a goto label **)
  | KEYWORD_GOTO id=located(identifier_var)
    { IGoto id }
  (** a set variable instruction **)
  | KEYWORD_SET sc=located(scope) var=located(identifier_var) e=located(expr)
    { ISet (sc, var, e) }
  (** a if not set variable instruction **)
  | KEYWORD_IFNSET sc=located(scope) var=located(identifier_var) e=located(expr)
    { IIfnset (sc, var, e) }
  (** a wait instruction **)
  | KEYWORD_WAIT n=located(number)
    { IWait n }
  (** a nop (do nothing) **)
  | KEYWORD_NOP
    { INop }
  (** a print message **)
  | msg = located(message)
    { IMessage msg }
  (** a choice **)
  | OPERATOR_CHOICE INDENT options=list(choiceoption) OUTDENT
    { IChoice(options) }
  (** a condition **)
  | OPERATOR_CHOICE condition=located(expr) INDENT branches=list(branch) OUTDENT
    { ICondition(condition, branches) }

(** a possible choice **)
choiceoption:
  | OPERATOR_CHOICEOPTION str=located(message) INDENT s=located(subprogram) OUTDENT
    { (str, s) }

(* Pattern matching *)
branch:
  | p=located(pattern) INDENT s=located(subprogram) OUTDENT
    { (p, s) }

pattern:
  (** a wildcard **)
  | OPERATOR_CHOICEOPTION OPERATOR_WILDCARD
    { PWildcard }
  (** a simple value **)
  | OPERATOR_CHOICEOPTION value=located(expr)
    { PValue value }
  (** a binding pattern **)
  | OPERATOR_CHOICEOPTION id=located(identifier_var) KEYWORD_WHEN e=located(expr)
    { PBinding (id, e) }

(* Expressions *)
expr:
  | lit = located(literal)
    { ELiteral(lit) }
  | var=located(identifier_var)
    { EVar(var) }
  | PUNCTUATION_LPAREN e=expr PUNCTUATION_RPAREN
    { e }

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
  | OPERATOR_STRING str=stringcontents OPERATOR_STRING { LString str }

number:
  | i = LITERAL_INT { LInt i }
  | f = LITERAL_FLOAT { LFloat f }
  | p = LITERAL_INT PUNCTUATION_PERCENT { LFloat ((Int32.to_float p) /. 100.) }

(* Messages & options*)
message:
  | OPERATOR_MESSAGE str=located(stringcontents) OPERATOR_MESSAGE
    { (str, []) }
  | OPERATOR_MESSAGE str=located(stringcontents) OPERATOR_MESSAGE opts=messageopts
    { (str, opts) }

messageopts:
  | opt = located(messageopt)
    { [opt] }
  | opt = located(messageopt) opts=messageopts
    { opt::opts }

messageopt:
  | KEYWORD_NORUSH
    { MsgNoRush }
  | KEYWORD_NOACK
    { MsgNoAcknowledge }

(* String tokens *)
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
