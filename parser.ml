module type IO = sig
  type 'a promise

  type t

  val return : 'a -> 'a promise

  val read_into : t -> bytes -> int -> int promise
end

module type P = sig
  type 'a t

  type 'a promise

  type input

  val parse : input -> 'a t -> ('a, string) result promise

  (** {2 Parsers} *)

  val char : char -> char t
end

module Make (Io : IO) :
  P with type 'a promise := 'a Io.promise with type input := Io.t = struct
  type input_buffer =
    { input : Io.t
    ; buf : bytes
    ; mutable pos : int
    }

  type 'a result = ('a, string) Result.t

  type 'a t =
       input_buffer
    -> succ:('a -> 'a result Io.promise)
    -> fail:(string -> 'a result Io.promise)
    -> 'a result Io.promise

  let parse (input : Io.t) (p : 'a t) =
    let ib = { input; buf = Bytes.create 0; pos = 0 } in
    p ib ~succ:(fun a -> Io.return (Ok a)) ~fail:(fun e -> Io.return (Error e))

  let char : char -> char t =
   fun c pb ~succ ~fail ->
    if Bytes.unsafe_get pb.buf pb.pos = c then (
      pb.pos <- pb.pos + 1;
      succ c
    ) else
      fail (Printf.sprintf "char %C" c)
end

module String_parser = Make (struct
  type 'a promise = 'a

  type t = string

  let return a = a

  let read_into t buf len =
    String.blit t 0 buf 0 len;
    len
end)

module Lwt_parser = Make (struct
  type 'a promise = 'a Lwt.t

  type t = Lwt_io.input_channel

  let return = Lwt.return

  let read_into t buf len = Lwt_io.read_into t buf 0 len
end)
