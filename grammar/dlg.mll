{
  open Lexing
  open DlgAST
  open DlgParser
  open Error
  open Position

  let next_line_and f lexbuf =
    Lexing.new_line lexbuf;
    f lexbuf

  let error lexbuf =
    error "during lexing" (lex_join lexbuf.lex_start_p lexbuf.lex_curr_p)

  (*
   *  COUNTERS
   *)
  (* count the amount of whitespace in each line*)
  let last_line_incr = ref 0
  let cur_line_incr = ref 0
  let incr_amount = ref 0
  (* counts the nesting of comments*)
  let comment_ctr = ref 0
  (* do we have to depile a string ? *)
  let string_tokens = ref []

  (*
   *  PARSING FUNCTIONS
   *)

  (*
   *  HELPERS
   *)
   type strparsemode = ModeMessage | ModeLiteral

   let ignore_line () =
     cur_line_incr := !(last_line_incr)

   let string_append token =
     (* check token type *)
     string_tokens := begin match token with
      (* try to fuse*)
      | STRING_CONST s2 -> begin match !string_tokens with
          (* if there is a prior token of type STRING_CONST, then concat*)
          | (STRING_CONST s1)::t -> (STRING_CONST (s1 ^ s2))::t
          (* just append *)
          | _ -> (token::(!string_tokens))
        end
      (* just append *)
      | _ -> (token::(!string_tokens))
    end

    let string_close closingtok =
      string_append closingtok;
      string_tokens := (List.rev !string_tokens)

}

(**
 * REGEXPS
 *)

(* Blanks & newlines *)
let newline = ('\010' | '\013' | "\013\010")
let space = [' ']
let whitespace = ['\009' '\012']
let blank   = space | whitespace

(* Comments *)
let comment = ';'

(* Identifiers *)
let id_dlg_const  = ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '_' '0'-'9']*
let id_dlg_object = id_dlg_const '.' id_dlg_const ('.' id_dlg_const)*

(* Literals *)
let lit_integer =   ('-'?['0'-'9']+)
              | ("0x"['0'-'9' 'a'-'f' 'A'-'F']+)
              | ("0b"['0'-'1']+)
              | ("0o"['0'-'7']+)
let lit_float =   '-'?['0'-'9']+'.'['0'-'9']+
let lit_stringatom    = ['\x20'-'\x7E']#['"'] (*printable*)

let lit_stringescape    = "n" | "b" | "t" | "r" | "\\" | "\""
let lit_stringescapenum = ("0x"['0'-'9' 'a'-'f' 'A'-'F']['0'-'9' 'a'-'f' 'A'-'F'])
                          | (['0'-'1']['0'-'9']['0'-'9'] |
                            "2"['0'-'4']['0'-'9'] |
                            "25"['0'-'5'])

(* Message specials *)
let colorhash_digit = ['0'-'9' 'a'-'f' 'A'-'F']
let colorhash =   ""
                | colorhash_digit colorhash_digit colorhash_digit
                | colorhash_digit colorhash_digit colorhash_digit colorhash_digit colorhash_digit colorhash_digit

(*
* RULES & TOKENS
*)

(* Main rule for matching tokens AND indents *)
rule main iseof = parse
  | ""
    {
      (* we need to empty the string token buffer*)
      if (!string_tokens) <> [] then
        (* deconstruct, remove the token, save and return it *)
        let tok = match (!string_tokens) with
          | h::t -> string_tokens := t; h
          | _ -> error lexbuf "Empty list, should not be reached"
        in tok
      (* we need to produce INDENT/OUTDENT tokens *)
      else if (!incr_amount) <> 0 then (incrproduce false lexbuf)
      (* no production needed. parse a regular token  *)
      else (token false lexbuf)
    }
and token isinline = parse
  (* Layout *)
  | newline         { next_line_and incrcount lexbuf }
  | comment         { comments lexbuf }
  | blank+          { token isinline lexbuf }
  | eof
    {
      (* set to produce as much deindent tokens as we are indented rn *)
      incr_amount := -(!cur_line_incr);
      (* produce them in EOF mode (produces an EOF at the end) *)
      incrproduce true lexbuf
    }

  (* Keywords *)
  | "label"          { KEYWORD_LABEL }
  | "goto"           { KEYWORD_GOTO }
  | "set"            { KEYWORD_SET }
  | "ifnset"         { KEYWORD_IFNSET }
  | "global"         { KEYWORD_GLOBAL }
  | "extern"         { KEYWORD_EXTERN }
  | "local"          { KEYWORD_LOCAL }
  | "wait"           { KEYWORD_WAIT }
  | "nop"            { KEYWORD_NOP }
  | "when"           { KEYWORD_WHEN }
  | "norush"         { KEYWORD_NORUSH }
  | "noack"         { KEYWORD_NOACK }
  | "invoke"         { KEYWORD_INVOKE }
  | "send"         { KEYWORD_SEND }
  | "speed"         { KEYWORD_SPEED }
  | "then"         { KEYWORD_THEN }

  (* Literals *)
  | ("true"|"false") as s     { LITERAL_BOOL (bool_of_string s ) }
  | lit_integer as s          { LITERAL_INT (Int32.of_string s) }
  | lit_float as s            { LITERAL_FLOAT (float_of_string s)}
  | "vec2"                    { LITERAL_VEC2 }
  | "vec3"                    { LITERAL_VEC3 }
  | "enum"                    { LITERAL_ENUM }

  (* Operators *)
  | "?="                 { OPERATOR_TERNARY }
  | '?'             { OPERATOR_CHOICE }
  | "->"            { OPERATOR_OPTION }
  | '"'
    {
      (* if we are inline, just send symbol*)
      if isinline then OPERATOR_MESSAGE
      (* if we are not inline, open string parser in msg mode*)
      else begin
          string_append OPERATOR_MESSAGE;
          message ModeMessage lexbuf
        end
    }
  | '\''
    {
      (* if we are inline, just send symbol*)
      if isinline then OPERATOR_STRING
      (* if we are not inline, open string parser in literal mode*)
      else begin
          string_append OPERATOR_STRING;
          message ModeLiteral lexbuf
        end
    }
  | '_'             { OPERATOR_WILDCARD }

  (* Identifiers *)
  | id_dlg_object as id { ID_OBJECT id }
  | id_dlg_const as id  { ID_VAR id  }

  (* Operations *)
  | "+"                  { OPERATION_PLUS   OpPlus }
  | "-"                  { OPERATION_MINUS  OpMinus }
  | "*"                  { OPERATION_STAR   OpStar }
  | "/"                  { OPERATION_DIVIDE OpDivide }
  | "&&"                 { OPERATION_AND    OpAnd }
  | "||"                 { OPERATION_OR     OpOr }
  | "=="                 { OPERATION_ISEQ   OpEqual }
  | "="                  { OPERATION_ISEQ   OpEqual }
  | "!="                 { OPERATION_ISNEQ  OpNotEqual }
  | "<="                 { OPERATION_LEQ    OpLeq }
  | ">="                 { OPERATION_GEQ    OpGeq }
  | "<"                  { OPERATION_LESS   OpLess }
  | ">"                  { OPERATION_MORE   OpMore }

  (* Punctuation *)
  | '%'             { PUNCTUATION_PERCENT }
  | '('             { PUNCTUATION_LPAREN }
  | ')'             { PUNCTUATION_RPAREN }
  | '['             { PUNCTUATION_LSQBRACKET }
  | ']'             { PUNCTUATION_RSQBRACKET }
  | '.'             { PUNCTUATION_DOT }
  | ','             { PUNCTUATION_COMMA }
  | ':'             { PUNCTUATION_COLON }
  | '#'             { PUNCTUATION_HASH }

  (* Closing a string inline *)
  | '$'
    {
      if isinline then STRING_INLINE
      else error lexbuf "inline character detected while not in a string"
    }

  (* Lexing error. *)
  | _               { error lexbuf "unexpected character." }


(* Rules for parsing strings *)
and message mode = parse
  (* closing the message *)
  | '"'
    {
      (* check mode *)
      match mode with
        (* we are parsing a message. this is close*)
        | ModeMessage -> begin
          (* close. *)
          string_close OPERATOR_MESSAGE;
          (* we go back to the main rule that'll produce all tokens *)
          main false lexbuf
          end
        (* we are parsing a literal. this is just a char*)
        | ModeLiteral -> begin
          (* a string constant *)
          string_append (STRING_CONST (Lexing.lexeme lexbuf));
          (* keep parsing *)
          message mode lexbuf
          end
    }
  | '\''
    {
      (* check mode *)
      match mode with
        (* we are parsing a message. this is close*)
        | ModeLiteral -> begin
          (* close. *)
          string_close OPERATOR_STRING;
          (* we go back to the main rule that'll produce all tokens *)
          main false lexbuf
          end
        (* we are parsing a message. this is just a char*)
        | ModeMessage -> begin
          (* a string constant *)
          string_append (STRING_CONST (Lexing.lexeme lexbuf));
          (* keep parsing *)
          message mode lexbuf
          end
    }
  (* entering an inline expression *)
  | "$"
    {
      (* append an entering string tag*)
      string_append STRING_INLINE;
      (* declare a token holder *)
      let tok = ref EOF in
      (* while we don't encounter a closing inline token '$' while parsing with 'token' rule*)
      while tok := (token true lexbuf); !tok <> STRING_INLINE
      (* append the token to the string constant*)
      do string_append (!tok); done;
      (* append a closing string tag*)
      string_append STRING_INLINE;
      (* keep parsing string *)
      message mode lexbuf
    }

  | "\n"" "*
    {
      (* if we are in message mode*)
      match mode with
        | ModeMessage -> begin
          (* add a newline string constant *)
          string_append (STRING_CONST "\n");
          (* increment the line in the lexer *)
          Lexing.new_line lexbuf;
          (* keep parsing *)
          message mode lexbuf
          end
        | ModeLiteral -> error lexbuf "newline inside string literal"
    }

  (* a color tag TODO : won't work because stringatom matches "\c" *)
  | "\\c{" '#'? (colorhash as s) "}"
    {
      (* check value of s *)
      let () = match s with
        (* no color specified ; this is a reset color cmd*)
        | "" -> string_append (STRING_COLOR None)
        (* a color has been specified ; this is a set color cmd*)
        | _ -> string_append (STRING_COLOR (Some s))
      in
      (* keep parsing *)
      message mode lexbuf
    }

  (* an escape character with a number*)
  | "\\" (lit_stringescapenum as s)
    {
      (* a string constant *)
      string_append (STRING_CONST (String.make 1 (Char.chr (int_of_string s))));
      (* keep parsing *)
      message mode lexbuf
    }
  (* an escape character *)
  | "\\" (lit_stringescape as s)
    {
      (* compute the escape char*)
      let escape = match s with
        | 'n' -> "\n"
        | 'b' -> "\b"
        | 't' -> "\t"
        | 'r' -> "\r"
        | '\\' -> "\\"
        | '"' -> "\""
        | _ -> error lexbuf "invalid escape character"
      in
      (* a string constant *)
      string_append (STRING_CONST escape);
      (* keep parsing *)
      message mode lexbuf
    }
  (* any other wrong escape character*)
  | ("\\" _) as s
    {
      error lexbuf (s ^ " : invalid escape character")
    }
  (* an atomic string part *)
  | lit_stringatom as s
    {
      (* a string constant *)
      string_append (STRING_CONST (String.make 1 s));
      (* keep parsing *)
      message mode lexbuf
    }
  (* a character *)
  | _               { error lexbuf "invalid character in string" }

(* Rule for incrementing INDENT counter *)
and incrcount = parse

  (* if comment, ignore whole comment and continue*)
  | blank* comment { comments lexbuf }
  (* if newline, we ignore the whole line*)
  | blank* newline { next_line_and incrcount lexbuf }

  (* proper indent spaces *)
  | space space
    {
      (* increment the current line counter*)
      cur_line_incr := (!cur_line_incr) + 1;
      (* keep counting spaces *)
      incrcount lexbuf
    }
  (* lone space, error *)
  | space      { error lexbuf "lonely ident space detected (line does not start with a pairwise number of spaces)"  }
  (* unacceptable whitespace (tabs...) *)
  | whitespace { error lexbuf "only spaces can be used for indentation" }
  (* we're done parsing indentation. consume no input *)
  | ""
    {
      (* we're done parsing indentation for this line. compute number tokens produced*)
      let amount = !(cur_line_incr) - !(last_line_incr) in
        (* iterate indentation for this line *)
        last_line_incr := !(cur_line_incr);
        (* reset for current line *)
        cur_line_incr := 0;
        (* set to make (current - amount) tokens *)
        incr_amount := amount;
        (* go produce them *)
        incrproduce false lexbuf
    }

(* Rule for making INDENT/dedent tokens*)
and incrproduce iseof = parse
  (* recursively produces indent and outdent tokens. consumes no input *)
  | ""
    {
      match (!incr_amount) with
        (* no more tokens to produce, go back to parsing*)
        | 0 -> if iseof then EOF  (* EOF mode on : after that, we end stream *)
               else token false lexbuf  (* EOF mode off : keep parsing tokens because there is more  *)
        (* we need to produce indent tokens *)
        | n when n > 0 -> incr_amount := ((!incr_amount) - 1); INDENT
        (* we need to produce dedent tokens *)
        | n when n < 0 -> incr_amount := ((!incr_amount) + 1); OUTDENT
        (* case unused because of include guards *)
        | _ -> error lexbuf "error"
    }

and comments = parse
  (* if newline, we continue parsing *)
  | newline
    {
      (* go to next line *)
      next_line_and incrcount lexbuf
    }
  (* ignore all text in a comment *)
  | _               { comments lexbuf }
