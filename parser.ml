module type IO = sig
  type t

  type 'a promise

  val return : 'a -> 'a promise

  val bind : 'a promise -> ('a -> 'b promise) -> 'b promise

  val read_into : t -> bytes -> int -> int promise
end

module type PARSER = sig
  type 'a t

  type 'a promise

  type input

  val parse : input -> 'a t -> ('a, string) result promise

  val bind : 'a t -> ('a -> 'b t) -> 'b t

  module Infix : sig
    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

    val ( *> ) : _ t -> 'b t -> 'b t
  end

  include module type of Infix

  (** {2 Parsers} *)

  val char : char -> char t
end

module Make (Io : IO) :
  PARSER with type 'a promise := 'a Io.promise with type input := Io.t = struct
  type input_buffer =
    { input : Io.t
    ; buf : bytes
    ; mutable pos : int
    }

  type 'a t =
       input_buffer
    -> succ:('a -> unit Io.promise)
    -> fail:(string -> unit Io.promise)
    -> unit Io.promise

  let bind : 'a t -> ('a -> 'b t) -> 'b t =
   fun p f ib ~succ ~fail -> p ib ~succ:(fun a -> f a ib ~succ ~fail) ~fail

  module Infix = struct
    let ( >>= ) = bind

    let ( *> ) : _ t -> 'b t -> 'b t = fun p q -> p >>= fun _ -> q
  end

  include Infix

  let parse (input : Io.t) (p : 'a t) =
    let ib = { input; buf = Bytes.create 0; pos = 0 } in
    let v = ref (Error "") in
    Io.bind
      (p ib
         ~succ:(fun a -> Io.return (v := Ok a))
         ~fail:(fun e -> Io.return (v := Error e)))
      (fun () -> Io.return !v)

  let ensure_input : int -> unit t =
   fun n ib ~succ ~fail ->
    if n + ib.pos <= Bytes.length ib.buf then
      succ ()
    else
      Io.bind (Io.read_into ib.input ib.buf n) (fun n ->
          if Int.equal n 0 then
            fail "not enough input"
          else
            succ ())

  let char : char -> char t =
   fun c ->
    let p : char t =
     fun ib ~succ ~fail ->
      if Bytes.unsafe_get ib.buf ib.pos = c then (
        ib.pos <- ib.pos + 1;
        succ c
      ) else
        fail (Printf.sprintf "char %C" c)
    in
    ensure_input 1 *> p
end

module String_parser = Make (struct
  type 'a promise = 'a

  type t = string

  let return a = a

  let bind promise f = f promise

  let read_into t buf len =
    String.blit t 0 buf 0 len;
    len
end)

module Lwt_parser = Make (struct
  type 'a promise = 'a Lwt.t

  type t = Lwt_io.input_channel

  let return = Lwt.return

  let bind = Lwt.bind

  let read_into t buf len = Lwt_io.read_into t buf 0 len
end)
