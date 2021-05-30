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
end

module Make (Io : IO) :
  P with type 'a io = 'a Io.t with type channel = Io.channel = struct
  type channel = Io.channel

  type 'a io = 'a Io.t

  type state =
    { io : channel
    ; buf : bytes
    ; mutable pos : int
    }

  type 'a t = state -> 'a

  let parse (_channel : channel) (_p : 'a t) = assert false
end

module String_parser = Make (struct
  type 'a t = 'a

  type channel = string

  let return a = a

  let read channel b ~len =
    let s = String.sub channel 0 len in
    Bytes.blit_string s 0 b 0 len;
    len
end)

module Lwt_parser = Make (struct
  type 'a t = 'a Lwt.t

  type channel = Lwt_io.input_channel

  let return = Lwt.return

  let read channel bytes ~len = Lwt_io.read_into channel bytes 0 len
end)
