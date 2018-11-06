open Datastack
open Data
open Env
open Error
open Io
open Progbuf
open Lwt
open Alu

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
  (* peek top *)
  begin match int_of_char (progbuf.next ()) with
    (*
     * FLOW CONTROL & STACK/ENV MANAGEMENT
     *)
    (* MEM instr *)
    | 0x01 ->
        (* memorize *)
        cpu_mem cpu (Stack.top cpu.stack); return ()
    (* DUPL instr *)
    | 0x02 ->
        (* duplicate *)
        cpu_dupl cpu; return ()
    (* Deepen scope *)
    | 0x03 -> return ()
    (* Raise scope *)
    | 0x04 -> return ()
    (* Skip if not *)
    | 0x10 ->
      (* get the amount to jump *)
      let jumpamount = progbuf_read_int64 progbuf in
      let goto = (Int64.add (progbuf.pos ()) jumpamount) in
      (* get bool *)
      let isok = bool_of_data (Stack.pop cpu.stack) in
      (* if they strings, concat (for the moment)*)
      if not isok then progbuf.seek goto;
      (* return *)
      return ()
    (* Skip *)
    | 0x11 ->
      (* get the amount to jump *)
      let jumpamount = progbuf_read_int64 progbuf in
      let goto = (Int64.add (progbuf.pos ()) jumpamount) in
      (* if they strings, concat (for the moment)*)
      progbuf.seek goto;
      (* return *)
      return ()

    (*
     * INSTRUCTIONS
     *)
    (* A message *)
    | 0x23 ->
      (* compute the options *)
      let optcode = progbuf.next() in
      let options = List.rev_map (fun i -> match i with
        | 0 -> MsgNoRush
        | 1 -> MsgNoAcknowledge
        | _ -> assert false) (Utils.Buf.deconstruct_flag (int_of_char optcode)) in
      (* get a string form the stack *)
      let str = string_of_data (Stack.pop cpu.stack) in
      (* send message *)
      io_send_message io options str

    (* A choice *)
    | 0x28 ->
      (* get the number of choices *)
      let choices_num = int_of_data (Stack.pop cpu.stack) in
      (* list of choices *)
      let choices = ref [] in
      (* depop stack *)
      for _ = 1 to Int32.to_int choices_num
      (* depop a strings & add it *)
      do
        let choice = string_of_data (Stack.pop cpu.stack) in
        choices := (!choices @ [choice])
      done;
      (* send it to the io, and wait for push token *)
      io_ask_choice io !choices
      (* push *)
      >>= fun chosen -> Stack.push (data_of_int (Int32.of_int chosen)) cpu.stack; return ()

    (* A nop *)
    | 0x80 -> return ()

    (*
     * IDENTIFIERS
     *)
    | 0x60 | 0x61 | 0x62 as scopeid ->
      (* read a string from here *)
      let id = progbuf_read_string progbuf in
      (* get a scope *)
      let scope = match scopeid with
        | 0x60 -> Extern
        | 0x61 -> Global
        | 0x62 -> Local
        | _ -> raise (Vm_error { reason = VmUnrecognizedDeclarator })
      in
      (* push the identifier *)
      Stack.push (Identifier (scope, id)) cpu.stack;
      (* Return *)
      return ()

    (*
     * LITERALS
     *)
    (* An int value *)
    | 0x90 ->
      (* Push on stack *)
      Stack.push (data_of_int (progbuf_read_int32 progbuf)) cpu.stack; return()
    (* A float value *)
    | 0x91 ->
      (* Push on stack *)
      Stack.push (data_of_float (progbuf_read_float progbuf)) cpu.stack; return()
    (* A boolean *)
    | 0x92 ->
      (* Push on stack *)
      Stack.push (data_of_bool (progbuf_read_bool progbuf)) cpu.stack; return()
     (* A string value *)
    | 0x93 ->
      (* Push on stack *)
      Stack.push (data_of_string (progbuf_read_string progbuf)) cpu.stack; return()
    (* An enum *)
    | 0x94 -> assert false
    (* A vector 2 value *)
    | 0x95 ->
      (* pull two elements from stack *)
      let x = Stack.pop cpu.stack in
      let y = Stack.pop cpu.stack in
      (* check if they're floats (or ints for that matter) *)
      let vx = number_of_data x in
      let vy = number_of_data y in
      (* push the value *)
      Stack.push (data_of_vec2 vx vy) cpu.stack; return()
    (* A vector 3 value *)
    | 0x96 ->
      (* pull two elements from stack *)
      let x = Stack.pop cpu.stack in
      let y = Stack.pop cpu.stack in
      let z = Stack.pop cpu.stack in
      (* check if they're floats (or ints for that matter) *)
      let vx = number_of_data x in
      let vy = number_of_data y in
      let vz = number_of_data z in
      (* push the value *)
      Stack.push (data_of_vec3 vx vy vz) cpu.stack; return()

    (*
     *  EXPRESIONS
     *)
    (* Inline expressions *)
    | 0x9F ->
      (* pop a tok *)
      let data = Stack.pop cpu.stack in
      (* push a string *)
      Stack.push (data_of_string (inline_string_of_data data)) cpu.stack;
      (* Return *)
      return ()
    (* An operation *)
    | 0xA0 ->
      (* pop two toks *)
      let lhs = value_of_data (Stack.pop cpu.stack) in
      let rhs = value_of_data (Stack.pop cpu.stack) in
      (* Get operation *)
      let op = match int_of_char (progbuf.next ()) with
        | 0x00 -> OpPlus
        | 0x01 -> OpMinus
        | 0x02 -> OpStar
        | 0x03 -> OpDivide
        | 0x04 -> OpAnd
        | 0x05 -> OpOr
        | 0x06 -> OpEqual
        | 0x07 -> OpNotEqual
        | 0x08 -> OpLeq
        | 0x09 -> OpGeq
        | 0x0A -> OpLess
        | 0x0B -> OpMore
        | _ -> raise (Vm_error { reason = VmUnrecognizedOperation })
      in
      (* Compute *)
      let result = alu_compute lhs op rhs in
      (* Push onto stack *)
      Stack.push (data_of_value result) cpu.stack; return ()
    (* Unrecognized *)
    | _ -> raise (Vm_error { reason = VmUnrecognizedDeclarator })
  end
