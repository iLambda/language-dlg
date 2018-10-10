{
  open Lexing
  open Parser
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

  (*
   *  PARSING FUNCTIONS
   *)

  (*
   *  HELPERS
   *)
   let ignore_line () =
     cur_line_incr := !(last_line_incr)

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
let id_dlg_const  = ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '_' '0'-'9']+

(* Literals *)
let lit_integer =   ('-'?['0'-'9']+)
              | ("0x"['0'-'9' 'a'-'f' 'A'-'F']+)
              | ("0b"['0'-'1']+)
              | ("0o"['0'-'7']+)
let lit_float =   '-'?['0'-'9']+'.'['0'-'9']+

(*
* RULES & TOKENS
*)

(* Rule for matching a token *)
rule main iseof = parse
  | ""
    {
      if (!incr_amount) <> 0 then (incrproduce false lexbuf)
      else (token lexbuf)
    }
and token = parse
  (* Layout *)
  | newline         { next_line_and incrcount lexbuf }
  | comment         { comments false lexbuf }
  | blank+          { token lexbuf }
  | eof
    {
      (* set to produce as much deindent tokens as we are indented rn *)
      incr_amount := -(!cur_line_incr);
      (* produce them in EOF mode (produces EOF at the end) *)
      incrproduce true lexbuf
    }

  (* Keywords *)
  | "label"          { KEYWORD_LABEL }
  | "goto"           { KEYWORD_GOTO }
  | "set"            { KEYWORD_SET }
  | "ifnset"         { KEYWORD_IFNSET }
  | "global"         { KEYWORD_GLOBAL }
  | "local"          { KEYWORD_LOCAL }
  | "wait"           { KEYWORD_WAIT }
  | "nop"            { KEYWORD_NOP }

  (* Literals *)
  | ("true"|"false") as s     { LITERAL_BOOL (bool_of_string s ) }
  | lit_integer as s          { LITERAL_INT (Int32.of_string s) }
  | lit_float as s            { LITERAL_FLOAT (float_of_string s)}

  (* Identifiers *)
  | id_dlg_const as id  { ID_VAR(id) }

  (* Operators *)
  | '?'             { OPERATOR_CHOICE }
  | '-'             { OPERATOR_CHOICEOPTION }
  | '"'             { message lexbuf }

  (* Punctuation *)
  | '%'             { PUNCTUATION_PERCENT }

  (* Lexing error. *)
  | _               { error lexbuf "unexpected character." }


(* Rules for parsing messages *)
and message = parse
  (* closing the message *)
  | '"'             { OPERATOR_MESSAGE "" } (*TODO CHANGE*)
  (* an escape character *)
  (* return to default token parsing *)
  | ""              { token lexbuf }
  (* a character *)
  | _               { token lexbuf }


(* Rule for incrementing INDENT counter *)
and incrcount = parse
  (* proper indent spaces *)
  | space space
    {
      (* increment the current line counter*)
      cur_line_incr := (!cur_line_incr) + 1;
      incrcount lexbuf
    }
  (* lone space, error *)
  | space      { error lexbuf "lonely ident space detected (line does not start with a pairwise number of spaces)"  }
  (* unacceptable whitespace (tabs...) *)
  | whitespace { error lexbuf "only spaces can be used for indentation" }
  (* we're done parsing indentation. consumes no input *)
  | ""
    {
      (* we're done parsing indentation for this line. compute number tokens produced*)
      let amount = !(cur_line_incr) - !(last_line_incr) in
        last_line_incr := !(cur_line_incr);
        cur_line_incr := 0;
        incr_amount := amount;
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
               else token lexbuf  (* EOF mode off : keep parsing tokens because there is more  *)
        (* we need to produce indent tokens *)
        | n when n > 0 -> incr_amount := ((!incr_amount) - 1); INDENT (*recursive here*)
        (* we need to produce dedent tokens *)
        | n when n < 0 -> incr_amount := ((!incr_amount) + 1); OUTDENT (*recursive here*)
        (* error *)
        | _ -> error lexbuf "error"
    }

and comments linefull = parse
  (* if newline, we continue parsing *)
  | newline
    {
      if linefull then ignore_line ()
      else ();
      next_line_and incrcount lexbuf
    }
  (* ignore all *)
  | _               { comments linefull lexbuf }
