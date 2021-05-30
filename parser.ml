module type IO = sig
  type 'a t

  type channel

  val read : channel -> bytes -> offset:int -> len:int -> int t
end

module type P = sig
  type 'a state

  type ('a, 'b) t = 'a state -> 'b
end

module Make (Io : IO) : P = struct
  type 'a state =
    { io : 'a Io.t
    ; buf : bytes
    ; mutable pos : int
    }

  type ('a, 'b) t = 'a state -> 'b
end

module String_parser = Make (struct
  type 'a t = 'a

  type channel = string

  let read channel b ~offset ~len =
    let s = String.sub channel offset len in
    Bytes.blit_string s 0 b offset len;
    len
end)
