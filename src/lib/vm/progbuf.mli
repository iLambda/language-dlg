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
val progbuf_from_file : in_channel -> progbuf
