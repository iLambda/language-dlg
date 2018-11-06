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
(* read int64 *)
val progbuf_read_int64 : progbuf -> int64
(* read int32 *)
val progbuf_read_int32 : progbuf -> int32
(* read float *)
val progbuf_read_float : progbuf -> float
(* read bool *)
val progbuf_read_bool : progbuf -> bool
(* read string *)
val progbuf_read_string : progbuf -> string
