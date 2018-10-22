(* Writes an int32 to a buffer *)
let int32_to_buf value =
  (* make a buffer *)
  let buf = Buffer.create 4 in
  (* loop four times *)
  for i = 0 to 3 do
    (* get the nibble *)
    let nibble = Int32.logand 255l (Int32.shift_right value (i * 8)) in
    (* put in buffer *)
    Buffer.add_char buf (Char.chr (Int32.to_int nibble));
  done;
  (* Return buffer *)
  buf

(* Writes a float to a buffer *)
let float_to_buf value =
  int32_to_buf (Int32.bits_of_float value)

(* Writes a boolean to a buffer *)
let bool_to_buf value =
  let buf = Buffer.create 1 in
  Buffer.add_char buf (if value then '\xFF' else '\x00');
  buf

(* Create a buffer from bytes *)
let from_bytes bytes =
  (* Make the buffer *)
  let buf = Buffer.create (Bytes.length bytes) in
  (* Fill it *)
  Buffer.add_bytes buf bytes;
  (* Return *)
  buf

(* Create a buffer from a byte list *)
(* val from_byte_list : int list -> Buffer.t *)
let from_byte_list bytes =
  (* Make the buffer *)
  let buf = Buffer.create (List.length bytes) in
  (* Fill it *)
  List.iter (Buffer.add_char buf) (List.map Char.chr bytes);
  (* Return *)
  buf

(* Create a byte from a list *)
let byte_from_list bytelist =
  (* Make the bytes *)
  let bytes = Bytes.create (List.length bytelist) in
  (* Fill it *)
  List.iteri (Bytes.set bytes) (List.map Char.chr bytelist);
  (* Return *)
  bytes
