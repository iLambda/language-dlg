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
  mutable mem: data option;
  mutable jumptable: (int32*int64) list option
}

(* Makes a cpu *)
let cpu_make () = {
  progbuf = None;
  stack = datastack_make ();
  environment = env_make ();
  mem= None;
  jumptable=None;
}

(* Bind a progbuf *)
let cpu_bind cpu progbuf =
  (* reset jumptable *)
  cpu.jumptable <- None;
  cpu.progbuf <- Some progbuf

(* push data in mem *)
let cpu_mem cpu data =
  cpu.mem <- Some data

(* duplicate memorized element in stack *)
let cpu_dupl cpu =
  (* check if mem full *)
  match cpu.mem with
    | None -> raise (Vm_error { reason = VmMemEmpty })
    | Some v -> Stack.push v cpu.stack

(* parse an operator *)
let parse_operation stack progbuf =
  (* pass the operation declarator and get the op code *)
  let opcode = ignore (progbuf.next ()); progbuf.next () in
  (* match *)
  match int_of_char (opcode) with
    | 0x00 ->
      (* pull two tokens *)
      let x = Stack.pop stack in
      let y = Stack.pop stack in
      (* if they strings, concat (for the moment)*)
      begin match x, y with
        | Value (VString s1), Value (VString s2) -> Value (VString (s1 ^ s2))
        | _ -> failwith "Not implemented"
      end
    | 0x06 ->
      (* pull two tokens *)
      let x = Stack.pop stack in
      let y = Stack.pop stack in
      (* if they values, compare*)
      begin match x, y with
        | Value v1, Value v2 -> Value (VBool (v1 = v2))
        | _ -> failwith "Not implemented"
      end
    | _ -> failwith "Not implemented"

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
let parse_value stack progbuf =
  (* get the value type *)
  let valuetype = int_of_char (progbuf.next ()) in
  (* check the type of the value *)
  match valuetype with
    (* An int value *)
    | 0x90 ->
      (* a reference for the value *)
      let intref = ref 0l in
      for i=0 to 3 do
        (* byte to int32 *)
        let nibble = Int32.of_int (int_of_char (progbuf.next ())) in
        (* add *)
        intref := Int32.logor !intref (Int32.shift_left nibble (i*8));
      done;
      (* return *)
      Value (VInt !intref)

    (* An int value *)
    | 0x91 ->
      (* a reference for the value *)
      let intref = ref 0l in
      for i=0 to 3 do
        (* byte to int32 *)
        let nibble = Int32.of_int (int_of_char (progbuf.next ())) in
        (* add *)
        intref := Int32.logor !intref (Int32.shift_left nibble (i*8));
      done;
      (* return *)
      Value (VFloat (Int32.float_of_bits !intref))

    (* A bool value *)
    | 0x92 ->
      (* return *)
      Value (VBool ((progbuf.next ()) <> '\x00'))

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

    (* A vector 2 value *)
    | 0x95 ->
      (* pull two elements from stack *)
      let x = Stack.pop stack in
      let y = Stack.pop stack in
      (* check if they're floats (or ints for that matter) *)
      begin match x, y with
        | Value (VFloat fx), Value (VFloat fy) -> Value (VVec2 (fx, fy))
        | Value (VInt fx), Value (VFloat fy) -> Value (VVec2 (Int32.to_float fx, fy))
        | Value (VFloat fx), Value (VInt fy) -> Value (VVec2 (fx, Int32.to_float fy))
        | Value (VInt fx), Value (VInt fy) -> Value (VVec2 (Int32.to_float fx, Int32.to_float fy))
        | _ -> raise (Vm_error { reason = VmIllFormedProgram })
      end

    (* An inline expression ended *)
    | 0x9F ->
      (* pull one element from stack *)
      let v = Stack.pop stack in
      (* check if value *)
      begin match v with
        | Value v -> Value (VString (string_of_value v))
        | _ -> raise (Vm_error { reason = VmIllFormedProgram })
      end

    (* Unrecognized *)
    | _ -> raise (Vm_error { reason = VmUnrecognizedDeclarator })

(* parse an instruction *)
let parse_instruction cpu io progbuf = match int_of_char (progbuf.next ()) with
  (* A message *)
  | 0x23 ->
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
  (* A choice *)
  | 0x28 ->
    (* get the number of choices *)
    begin match Stack.pop cpu.stack with
      | Value (VInt choices_num) ->
        (* list of choices *)
        let choices = ref [] in
        (* depop stack *)
        for _=0 to (Int32.to_int choices_num) - 1
        do
          (* depop a string *)
          let choice = match Stack.pop cpu.stack with
            | Value (VString s) -> s
            | _ -> raise (Vm_error { reason = VmIllFormedProgram })
          in
          (* add it *)
          choices := (!choices @ [choice])
        done;
        (* send it to the io, and wait for push token *)
        let chosen = io_ask_choice io !choices in
        (* push *)
        Stack.push (Value (VInt (Int32.of_int chosen))) cpu.stack
      (* Error.*)
      | _ -> raise (Vm_error { reason = VmIllFormedProgram })
    end


  (* Unrecognized *)
  | _ -> raise (Vm_error { reason = VmUnrecognizedDeclarator })

(* parse a command*)
let parse_command cpu progbuf = match int_of_char (progbuf.next ()) with
  | 0x01 -> cpu_mem cpu (Stack.top cpu.stack)
  | 0x02 -> cpu_dupl cpu
  | 0x03 | 0x04 -> ()
  | 0x10 ->
    (* get the amount to jump *)
    let jumpamount = ref 0L in
    for i=0 to 7 do
      (* byte to int64 *)
      let nibble = Int64.of_int (int_of_char (progbuf.next ())) in
      (* add *)
      jumpamount := Int64.logor !jumpamount (Int64.shift_left nibble (i*8));
    done;
    (* get token *)
    let isok = Stack.pop cpu.stack in
    (* if they strings, concat (for the moment)*)
    begin match isok with
      | Value (VBool false) -> progbuf.seek (Int64.add (progbuf.pos ()) !jumpamount)
      | Value (VBool true) -> ()
      | _ -> failwith "Not implemented"
    end

  | 0x11 ->
    (* get the amount to jump *)
    let jumpamount = ref 0L in
    for i=0 to 7 do
      (* byte to int64 *)
      let nibble = Int64.of_int (int_of_char (progbuf.next ())) in
      (* add *)
      jumpamount := Int64.logor !jumpamount (Int64.shift_left nibble (i*8));
    done;
    (* skip *)
    progbuf.seek (Int64.add (progbuf.pos ()) !jumpamount)
  | v -> print_int v; assert false

(* parse the jumptable *)
let parse_jumptable cpu progbuf =
  (* the read byte *)
  let current = ref 0 in
  (* read each byte till 0xFF *)
  while (current := (int_of_char (progbuf.next ()))); !current <> 0xFF
  do (); done;
  (* table is built *)
  cpu.jumptable <- Some []

(* start the cpu *)
let cpu_step cpu io =
  (* get the progbuf *)
  let progbuf = match cpu.progbuf with
    | None -> raise (Vm_error { reason = VmUnexpectedEOP })
    | Some p -> p
  in

  (* if jumptable not made, parse it *)
  if cpu.jumptable = None then parse_jumptable cpu progbuf;
  (* peek at top *)
  begin match int_of_char (progbuf.peek ()) with
    (* A special op *)
    | n when n < 0x20 -> parse_command cpu progbuf
    (* A nop *)
    | n when n = 0x80 -> ignore (progbuf.next ())
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
      Stack.push (parse_value cpu.stack progbuf) cpu.stack
    (* An operation *)
    | n when n = 0xA0 ->
      (* Push onto stack *)
      Stack.push (parse_operation cpu.stack progbuf) cpu.stack

    (* Unrecognized *)
    | _ -> raise (Vm_error { reason = VmUnrecognizedDeclarator })
  end
