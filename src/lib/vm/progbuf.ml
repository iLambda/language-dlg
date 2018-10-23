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
