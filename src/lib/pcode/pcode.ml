open DLG.Ast
open Stream

(* An opcode for the outputted p-code *)
type opcode =
  | OpcEop (* The end of a program *)

(* A p-code*)
type pcode = opcode list

(* Raised when *)
exception Pcode_end_of_program

(* Returns the p-code of a program *)
let of_program _program = [ OpcEop ]

(* Returns bytes from an opcode *)
let bytes_of_opcode opcode =
  (* construct the byte list *)
  let bytelist = match opcode with
    (* The end of the program *)
    | OpcEop -> raise Pcode_end_of_program
  in
  (* create a buffer *)
  let buffer = Buffer.create 16 in
  (* fill it  *)
  List.iter (fun charcode -> Buffer.add_char buffer (Char.chr charcode)) bytelist;
  (* Return bytes result *)
  Buffer.to_bytes buffer

(* Returns a stream of bytes *)
let to_byte_stream pcode =
  (* A reference containing the pcode *)
  let currentpcode = ref pcode in
  (* New element of stream *)
  let stream_new_element n = match !currentpcode with
    (* Empty pcode : signal end of stream *)
    | [] -> None
    (* Nonempty *)
    | opcode::tail ->
      (* Save tail *)
      currentpcode := tail;
      (* Try catch Pcode_end_of_program exception *)
      match bytes_of_opcode opcode with
        (* End of program => end of stream *)
        | exception Pcode_end_of_program -> None
        (* An op code *)
        | bytes -> Some bytes
  in
  (* Build a stream *)
  Stream.from stream_new_element

(* Returns an array of bytes *)
let to_bytes pcode =
  (* The buffer *)
  let buffer = Buffer.create 256 in
  (* The stream *)
  let stream = to_byte_stream pcode in
  (* Fill it *)
  Stream.iter (Buffer.add_bytes buffer) stream;
  (* Return the buffer in byte form *)
  Buffer.to_bytes buffer

(* Writes to an output channel *)
let to_out channel pcode =
  (* The stream *)
  let stream = to_byte_stream pcode in
  (* Send them all *)
  Stream.iter (output_bytes channel) stream

(* Writes to a file *)
let to_file filename pcode =
  (* Open the file *)
  let oc = open_out_gen [Open_wronly; Open_creat; Open_trunc; Open_binary] 0o777 filename in
  (* write in it *)
  to_out oc pcode
