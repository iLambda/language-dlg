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
%token KEYWORD_SEND
%token KEYWORD_EXTERN
%token KEYWORD_INVOKE
%token KEYWORD_SPEED
%token KEYWORD_THEN
%token KEYWORD_DECLARE

(* Types *)
%token TYPE_INT
%token TYPE_FLOAT
%token TYPE_BOOL
%token TYPE_STRING
%token TYPE_VEC2
%token TYPE_VEC3
%token TYPE_ENUM
%token TYPE_VOID

(* Literals *)
%token<bool> LITERAL_BOOL
%token<int32> LITERAL_INT
%token<float> LITERAL_FLOAT
(* Identifiers *)
%token<string> ID_OBJECT
%token<string> ID_VAR

(* Operators *)
%token OPERATOR_CHOICE
%token OPERATOR_MESSAGE
%token OPERATOR_OPTION
%token OPERATOR_WILDCARD
%token OPERATOR_STRING
%token OPERATOR_TERNARY

(* operation *)
%token <DlgAST.operation> OPERATION_PLUS
%token <DlgAST.operation> OPERATION_MINUS
%token <DlgAST.operation> OPERATION_STAR
%token <DlgAST.operation> OPERATION_DIVIDE
%token <DlgAST.operation> OPERATION_AND
%token <DlgAST.operation> OPERATION_OR
%token <DlgAST.operation> OPERATION_ISEQ
%token <DlgAST.operation> OPERATION_ISNEQ
%token <DlgAST.operation> OPERATION_LEQ
%token <DlgAST.operation> OPERATION_GEQ
%token <DlgAST.operation> OPERATION_LESS
%token <DlgAST.operation> OPERATION_MORE

(* Message *)
%token STRING_INLINE
%token<string> STRING_CONST
%token<string option> STRING_COLOR

(* Punctuation *)
%token PUNCTUATION_PERCENT
%token PUNCTUATION_LPAREN
%token PUNCTUATION_RPAREN
%token PUNCTUATION_COMMA
%token PUNCTUATION_COLON
%token PUNCTUATION_HASH
%token PUNCTUATION_LSQBRACKET
%token PUNCTUATION_RSQBRACKET
%token PUNCTUATION_DOT

%start<DlgAST.t> program


(*
 * PRIORITY RULES
 *)

(* the infix operators*)
%right OPERATOR_TERNARY PUNCTUATION_COLON
%left OPERATION_AND OPERATION_OR
%nonassoc OPERATION_ISEQ, OPERATION_ISNEQ, OPERATION_LEQ, OPERATION_GEQ, OPERATION_LESS, OPERATION_MORE
%left OPERATION_PLUS, OPERATION_MINUS
%left OPERATION_DIVIDE OPERATION_STAR
%nonassoc PUNCTUATION_DOT

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

  (** a set variable instruction for a script var**)
  | KEYWORD_SET var=located(extended_variable) e=located(expr)
    { ISet (var, e) }
  (** a if not set variable instruction **)
  | KEYWORD_IFNSET var=located(extended_variable) e=located(expr)
    { IIfnset (var, e) }

  (** a wait instruction **)
  | KEYWORD_WAIT n=located(expr)
    { IWait (None, n) }
  | KEYWORD_WAIT event=located(identifier_var) KEYWORD_THEN n=located(expr)
    { IWait (Some event, n) }

  (** a nop (do nothing) **)
  | KEYWORD_NOP
    { INop }
  (** a print message **)
  | msg = located(message)
    { IMessage msg }
  (** a choice **)
  | OPERATOR_CHOICE INDENT options=list(choice) OUTDENT
    { IChoice(options) }
  (** a condition **)
  | OPERATOR_CHOICE condition=located(expr) INDENT branches=list(branch) OUTDENT
    { ICondition(condition, branches) }
  (** a function invoke **)
  | KEYWORD_INVOKE func=located(identifier_obj) args=located(arg_list)
    { IInvoke (func, args) }
  (** a speed **)
  | KEYWORD_SPEED n=located(expr)
    { ISpeed n }
  (** a send command **)
  | KEYWORD_SEND id=located(identifier_obj) arg=option(located(expr))
    { ISend (id, arg) }
  (** a declare type command for global type **)
  | KEYWORD_DECLARE KEYWORD_GLOBAL id=located(identifier_var) t=located(type_const)
    {
      let var = Position.with_pos (Position.position id) (SGlobal, (Position.value id)) in
      IDeclare (var, t, None)
    }
  (** a declare type command for global type **)
  | KEYWORD_DECLARE KEYWORD_EXTERN id=located(identifier_obj) t=located(type_const) args=option(type_list)
    {
      let var = Position.with_pos (Position.position id) (SExtern, (Position.value id)) in
      IDeclare (var, t, args)
    }
  | KEYWORD_DECLARE KEYWORD_EXTERN id=located(identifier_var) t=located(type_const) args=option(type_list)
    {
      let var = Position.with_pos (Position.position id) (SExtern, (Position.value id)) in
      IDeclare (var, t, args)
    }

(** a possible choice **)
choice:
  | OPERATOR_OPTION str=located(message) INDENT s=located(subprogram) OUTDENT
    { (str, s) }

(* Pattern matching *)
branch:
  | OPERATOR_OPTION p=located(pattern) INDENT s=located(subprogram) OUTDENT
    { (p, s) }

pattern:
  (** a wildcard **)
  | OPERATOR_WILDCARD
    { PWildcard }
  (** a simple value **)
  | value=located(expr)
    { PValue value }
  (** a binding pattern **)
  | id=located(identifier_var) KEYWORD_WHEN e=located(expr)
    { PBinding (id, e) }

(* Expressions *)
expr:
  (* a literal *)
  | lit = located(literal)
    { ELiteral lit }
  (* a variable identifier *)
  | var=located(variable)
    { EVar var }
  (* bracketing expression *)
  | PUNCTUATION_LPAREN e=expr PUNCTUATION_RPAREN
    { e }
  | id=located(identifier_var) args=located(arg_list)
    { EFunc (id, args) }
  (* ternary *)
  | cond=located(expr) OPERATOR_TERNARY a=located(expr) PUNCTUATION_COLON b=located(expr)
    { ECondition (cond, a, b) }
  (* access operator *)
  | vec=located(expr) PUNCTUATION_DOT PUNCTUATION_LSQBRACKET coord=located(identifier_var) PUNCTUATION_RSQBRACKET
    { EAccess (vec, coord) }
  (* infix operators *)
  | lhs = located(expr) operator = located(OPERATION_PLUS) rhs = located(expr)
  | lhs = located(expr) operator = located(OPERATION_MINUS) rhs = located(expr)
  | lhs = located(expr) operator = located(OPERATION_STAR) rhs = located(expr)
  | lhs = located(expr) operator = located(OPERATION_DIVIDE) rhs = located(expr)
  | lhs = located(expr) operator = located(OPERATION_AND) rhs = located(expr)
  | lhs = located(expr) operator = located(OPERATION_OR) rhs = located(expr)
  | lhs = located(expr) operator = located(OPERATION_ISEQ) rhs = located(expr)
  | lhs = located(expr) operator = located(OPERATION_ISNEQ) rhs = located(expr)
  | lhs = located(expr) operator = located(OPERATION_LEQ) rhs = located(expr)
  | lhs = located(expr) operator = located(OPERATION_GEQ) rhs = located(expr)
  | lhs = located(expr) operator = located(OPERATION_LESS) rhs = located(expr)
  | lhs = located(expr) operator = located(OPERATION_MORE) rhs = located(expr)
    {
      (* check if expression is already an operation *)
      EOperation (operator, lhs, rhs)
      (*match Position.value rhs with
        (* recompose arguments *)
        | EOperation (o, args) when (Position.value o) = (Position.value operator) -> EOperation (operator, lhs::args)
        (* create a new operation *)
        | _                                                                        -> EOperation (operator, [lhs; rhs])
      *)
    }

(* Argument list *)
arg_list:
  | tuple=delimited(PUNCTUATION_LPAREN, separated_list(PUNCTUATION_COMMA, located(expr)), PUNCTUATION_RPAREN)
    { tuple }

(* Keywords *)
/* scope:
  | s = scope_script { s }
  | s = scope_runtime { s } */

scope_script:
  | KEYWORD_GLOBAL
    { SGlobal }
  | KEYWORD_LOCAL
    { SLocal }
scope_runtime:
  | KEYWORD_EXTERN
    { SExtern }

(* Variable *)
extended_variable:
  | var=variable
    { var }
  | sc=scope_runtime id=identifier_obj
    { (sc, id) }
  | sc=scope_script id=identifier_var
    { (sc, id) }

variable:
  | sc=scope_runtime PUNCTUATION_COLON id=identifier_obj
    { (sc, id) }
  | sc=scope_script PUNCTUATION_COLON id=identifier_var
    { (sc, id) }
  | id=identifier_var
    { (SLocal, id) }
  | PUNCTUATION_HASH id=identifier_var
    { (SGlobal, id) }
  | id=identifier_obj
    { (SExtern, id) }

(* Identifiers *)
identifier_var:
  | id = ID_VAR
    { Id id }

identifier_obj:
  | id = ID_OBJECT
    { Id id }

(* types *)
type_list:
  | args=delimited(PUNCTUATION_LPAREN, separated_list(PUNCTUATION_COMMA, located(type_const)), PUNCTUATION_RPAREN)
    { args }

type_const:
  | TYPE_VOID
    { TVoid }
  | TYPE_INT
    { TInt }
  | TYPE_BOOL
    { TBool }
  | TYPE_FLOAT
    { TFloat }
  | TYPE_STRING
    { TString }
  | TYPE_VEC2
    { TVec2 }
  | TYPE_VEC3
    { TVec3 }
  | TYPE_ENUM PUNCTUATION_LPAREN t=identifier_var PUNCTUATION_RPAREN
    { TEnum (match t with Id s -> s) }

(* Literals *)
literal:
  | b = LITERAL_BOOL { LBool b }
  | n = number       { n }
  | OPERATOR_STRING str=option(stringcontents) OPERATOR_STRING
    { LString (match str with | None -> [] | Some l -> l) }
  | TYPE_VEC2 PUNCTUATION_LPAREN x=located(expr) PUNCTUATION_COMMA y=located(expr) PUNCTUATION_RPAREN
    {
      LVec2 (x, y)
    }
  | TYPE_VEC3 PUNCTUATION_LPAREN x=located(expr) PUNCTUATION_COMMA y=located(expr) PUNCTUATION_COMMA z=located(expr) PUNCTUATION_RPAREN
    {
      LVec3 (x, y, z)
    }
  | TYPE_ENUM PUNCTUATION_LPAREN t=located(identifier_var) PUNCTUATION_COMMA value=located(identifier_var) PUNCTUATION_RPAREN
    {
      LEnum (t, value)
    }
number:
  | i = LITERAL_INT { LInt i }
  | f = LITERAL_FLOAT { LFloat f }
  | p = LITERAL_INT PUNCTUATION_PERCENT { LFloat ((Int32.to_float p) /. 100.) }

(* Messages & options*)
message:
  | OPERATOR_MESSAGE OPERATOR_MESSAGE
    { ([], []) }
  | OPERATOR_MESSAGE str=stringcontents OPERATOR_MESSAGE
    { (str, []) }
  | OPERATOR_MESSAGE str=stringcontents OPERATOR_MESSAGE opts=messageopts
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
