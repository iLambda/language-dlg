(* a progbuf *)
type progbuf = {
  (* Get the next byte *)
  next: (unit -> char);
  (* Seek a position *)
  seek: (int64 -> unit);
  (* Peek *)
  peek: (unit -> char);
  (* Empty *)
  eof: (unit -> bool);
  (* Counter *)
  pos: (unit -> int64)
}

(* create progbuf from file *)
let progbuf_from_file file = {
  next = (fun () -> input_char file);
  seek = (fun (p) -> seek_in file (Int64.to_int p));
  peek = (fun () ->
    let c = input_char file in
    seek_in file ((pos_in file) - 1);
    c);
  eof = (fun () ->
    try ignore (input_char file); seek_in file ((pos_in file) - 1); false
    with
      | End_of_file -> true
    );
  pos = (fun () -> Int64.of_int (pos_in file))
}

(* read int64 *)
let progbuf_read_int64 progbuf =
  (* get the amount to jump *)
  let value = ref 0L in
  for i=0 to 7 do
    (* byte to int64 *)
    let nibble = Int64.of_int (int_of_char (progbuf.next ())) in
    (* add *)
    value := Int64.logor !value (Int64.shift_left nibble (i*8));
  done;
  (* return *)
  !value

(* read int32 *)
let progbuf_read_int32 progbuf =
  let intref = ref 0l in
  for i=0 to 3 do
    (* byte to int32 *)
    let nibble = Int32.of_int (int_of_char (progbuf.next ())) in
    (* add *)
    intref := Int32.logor !intref (Int32.shift_left nibble (i*8));
  done;
  !intref

(* read float *)
let progbuf_read_float progbuf = Int32.float_of_bits (progbuf_read_int32 progbuf)

(* read bool *)
let progbuf_read_bool progbuf = (progbuf.next ()) <> '\x00'

(* read string *)
let progbuf_read_string progbuf =
  (* create a buffer and storage for the current byte *)
  let buf = Buffer.create 16 in
  let curbyte = ref '\x00' in
  (* fill it with incoming data*)
  while curbyte := progbuf.next (); !curbyte <> '\x00'
  do Buffer.add_char buf !curbyte
  done;
  (* return *)
  Utils.Buf.to_string buf
