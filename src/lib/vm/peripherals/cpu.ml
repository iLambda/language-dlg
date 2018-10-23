open Datastack
open Data
open Env
open Error
open Io
open Progbuf

type cpu = {
  mutable progbuf: progbuf option;
  stack: datastack;
  environment: env;
}

(* Makes a cpu *)
let cpu_make () = {
  progbuf = None;
  stack = datastack_make ();
  environment = env_make ();
}

(* Bind a progbuf *)
let cpu_bind cpu progbuf =
  cpu.progbuf <- Some progbuf

(* parse an identifier in the progbuf *)
let parse_identifier progbuf =
  (* get the scope type *)
  let scopetype = int_of_char (progbuf.next ()) in
  (* create a buffer and storage for the current byte *)
  let buf = Buffer.create 16 in
  let curbyte = ref '\x00' in
  (* fill it with incoming data*)
  while curbyte := progbuf.next (); !curbyte <> '\x00'
  do Buffer.add_char buf !curbyte
  done;
  (* get a scope *)
  match scopetype with
    (* can't use extern *)
    | 0x60 -> raise (Vm_error { reason = VmExternAccessUnsupported })
    (* global identifier *)
    | 0x61 -> Identifier (Global, (Utils.Buf.to_string buf))
    (* local identifier *)
    | 0x62 -> Identifier (Local, (Utils.Buf.to_string buf))
    (* Unrecognized *)
    | _ -> raise (Vm_error { reason = VmUnrecognizedDeclarator })

(* parse a literal *)
let parse_value progbuf =
  (* get the value type *)
  let valuetype = int_of_char (progbuf.next ()) in
  (* check the type of the value *)
  match valuetype with
    (* A string value *)
    | 0x93 ->
      (* create a buffer and storage for the current byte *)
      let buf = Buffer.create 32 in
      let curbyte = ref '\x00' in
      (* fill it with incoming data*)
      while curbyte := progbuf.next (); !curbyte <> '\x00'
      do Buffer.add_char buf !curbyte
      done;
      (* Return a string value *)
      Value (VString (Utils.Buf.to_string buf))
    (* Unrecognized *)
    | _ -> raise (Vm_error { reason = VmUnrecognizedDeclarator })

(* parse an instruction *)
let parse_instruction cpu io progbuf = match int_of_char (progbuf.next ()) with
  (* A message *)
  | 0x22 ->
    let optcode = progbuf.next() in
    (* compute the options *)
    let options = List.rev_map (fun i -> match i with
        | 0 -> MsgNoRush
        | 1 -> MsgNoAcknowledge
        | _ -> assert false) (Utils.Buf.deconstruct_flag (int_of_char optcode)) in
    (* pull the token from the stack and check if it is a string *)
    begin match Stack.pop cpu.stack with
      (* Print the message *)
      | Value (VString s) -> io_send_message io options s
      (* There should be a string here. Ill formed program *)
      | _ -> raise (Vm_error { reason = VmIllFormedProgram })
    end
  (* Unrecognized *)
  | _ -> raise (Vm_error { reason = VmUnrecognizedDeclarator })




(* start the cpu *)
let cpu_step cpu io =
  (* get the progbuf *)
  let progbuf = match cpu.progbuf with
    | None -> raise (Vm_error { reason = VmUnexpectedEOP })
    | Some p -> p
  (* peek at top *)
  in begin match int_of_char (progbuf.peek ()) with
    (* An instruction *)
    | n when n >= 0x20 && n < 0x60 ->
      (* Parse an instruction *)
      parse_instruction cpu io progbuf
    (* An identifier *)
    | n when n >= 0x60 && n < 0x70 ->
      (* Push onto stack *)
      Stack.push (parse_identifier progbuf) cpu.stack
    (* An value *)
    | n when n >= 0x90 && n < 0xA0 ->
      (* Push onto stack *)
      Stack.push (parse_value progbuf) cpu.stack
    (* Unrecognized *)
    | _ -> raise (Vm_error { reason = VmUnrecognizedDeclarator })
  end
