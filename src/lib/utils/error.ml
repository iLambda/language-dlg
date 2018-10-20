open Lexing

let exit_flag = ref true

let exit_on_error () = exit_flag := true

let resume_on_error () = exit_flag := false

exception Error of Position.t list * string

let print_syntax_error_at file lexbuf =
  let at_line p = p.pos_lnum in
  let line_pos p = p.pos_cnum - p.pos_bol in
  let get_line file p =
    let pos_start_line = p.pos_bol in
      (* set at beginning of line *)
      seek_in file pos_start_line;
      (* read line *)
      input_line file
  in let underline line start e =
    if start == e then line, "", ""
    else  (String.sub line 0 start), (String.sub line start (e-start)), (String.sub line e ((String.length line) - e))
  in let header =
    "Syntax error in line " ^ (string_of_int (at_line (Lexing.lexeme_start_p lexbuf))) ^
    ", characters " ^ (string_of_int (line_pos (Lexing.lexeme_start_p lexbuf))) ^ ":" ^  (string_of_int (line_pos (Lexing.lexeme_end_p lexbuf))) ^
    "\n" in
  let m_s, m_u, m_e = underline (get_line file (Lexing.lexeme_start_p lexbuf)) (line_pos (Lexing.lexeme_start_p lexbuf)) (line_pos (Lexing.lexeme_end_p lexbuf)) in
  ANSITerminal.print_string [] ("\n" ^ header ^ m_s);
  ANSITerminal.print_string [ANSITerminal.red; ANSITerminal.Underlined] m_u;
  ANSITerminal.print_string [] (m_e ^ "\n")

let print_error positions msg =
  Printf.sprintf "%s%s\n"
    (String.concat "\n"
       (List.map (fun p -> Position.string_of_pos p ^": ") positions))
    msg

let error_alert positions msg =
  if !exit_flag then (
    output_string stderr (print_error positions msg);
    exit 1
  )
  else raise (Error (positions, msg))

let global_error kind msg =
  error_alert [] (Printf.sprintf "Global Error (%s)\n  %s"  kind msg)

let errorN kind poss msg =
  error_alert poss (Printf.sprintf "Error (%s)\n  %s" kind msg)

let error kind pos = errorN kind [pos]
let error2 kind pos1 pos2 = errorN kind [pos1; pos2]
