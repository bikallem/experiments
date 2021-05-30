module type IO = sig
  type 'a t

  type channel

  val return : 'a -> 'a t

  val read : channel -> bytes -> len:int -> int t
end

module type P = sig
  type 'a t

  type channel

  type 'a io

  val parse : channel -> 'a t -> 'a io

  (** {2 Parsers} *)

  val next : char t
end

module Make (Io : IO) :
  P with type 'a io = 'a Io.t with type channel = Io.channel = struct
  type channel = Io.channel

  type 'a io = 'a Io.t

  type state =
    { channel : channel
    ; buf : bytes
    ; mutable buf_pos : int
    }

  type 'a t = state -> 'a io

  let parse (_channel : channel) (_p : 'a t) = assert false

  let rec next state =
    let buf_pos = state.buf_pos + 1 in
    if buf_pos >= Bytes.length state.buf then
      refill state
    else
      state.buf_pos <- buf_pos;
    Io.return (Bytes.unsafe_get state.buf state.buf_pos)

  and refill _state = ()
end

module String_parser = Make (struct
  type 'a t = 'a

  type channel = string

  let return a = a

  let read channel b ~len =
    String.blit channel 0 b 0 len;
    len
end)

module Lwt_parser = Make (struct
  type 'a t = 'a Lwt.t

  type channel = Lwt_io.input_channel

  let return = Lwt.return

  let read channel bytes ~len = Lwt_io.read_into channel bytes 0 len
end)
