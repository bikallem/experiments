module type INPUT = sig
  type t

  type 'a promise

  val length : t -> int

  val return : 'a -> 'a promise

  val bind : ('a -> 'b promise) -> 'a promise -> 'b promise

  val string : t -> pos:int -> len:int -> [ `String of string | `Eof ] promise
end

module type PARSER = sig
  type 'a t

  type 'a promise

  type input

  val parse : input -> 'a t -> ('a, string) result promise

  val bind : ('a -> 'b t) -> 'a t -> 'b t

  val map : ('a -> 'b) -> 'a t -> 'b t

  module Infix : sig
    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

    val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t

    val ( *> ) : _ t -> 'b t -> 'b t

    val ( <|> ) : 'a t -> 'a t -> 'a t
  end

  include module type of Infix

  (** {2 Parsers} *)

  val char : char -> char t
end

module Make (Input : INPUT) :
  PARSER with type 'a promise := 'a Input.promise with type input := Input.t =
struct
  type 'a t =
       Input.t
    -> pos:int
    -> succ:(pos:int -> 'a -> unit Input.promise)
    -> fail:(pos:int -> string -> unit Input.promise)
    -> unit Input.promise

  let bind f p input ~pos ~succ ~fail =
    p input ~pos ~succ:(fun ~pos a -> f a input ~pos ~succ ~fail) ~fail

  let map f p input ~pos ~succ ~fail =
    p input ~pos ~succ:(fun ~pos a -> succ ~pos (f a)) ~fail

  module Infix = struct
    let ( >>= ) p f = bind f p

    let ( >>| ) p f = map f p

    let ( *> ) : _ t -> 'b t -> 'b t = fun p q -> p >>= fun _ -> q

    let ( <|> ) : 'a t -> 'a t -> 'a t =
     fun p q input ~pos ~succ ~fail ->
      p input ~pos ~succ ~fail:(fun ~pos _s -> q input ~pos ~succ ~fail)
  end

  include Infix

  let parse (input : Input.t) (p : 'a t) =
    let v = ref (Error "") in
    p input ~pos:0
      ~succ:(fun ~pos:_ a -> Input.return (v := Ok a))
      ~fail:(fun ~pos:_ e -> Input.return (v := Error e))
    |> Input.bind (fun () -> Input.return !v)

  let input : int -> string t =
   fun n input ~pos ~succ ~fail ->
    Input.string input ~pos ~len:n
    |> Input.bind (function
         | `String s when String.length s = n -> succ ~pos s
         | `String _ -> fail ~pos "not enough input"
         | `Eof -> fail ~pos "not enough input")

  let char : char -> char t =
   fun c ->
    input 1
    >>= fun s _ ~pos ~succ ~fail ->
    if s.[0] = c then
      succ ~pos:(pos + 1) c
    else
      fail ~pos
        (Printf.sprintf "[char] pos: %d, expected %C, got %C" pos c s.[0])
end

module String_parser = Make (struct
  type 'a promise = 'a

  type t = string

  let return a = a

  let bind f promise = f promise

  let length t = String.length t

  let string t ~pos ~len =
    if pos + len < String.length t then
      `String (String.sub t pos len)
    else
      `Eof
end)

module Lwt_parser = Make (struct
  type 'a promise = 'a Lwt.t

  type t = Lwt_io.input_channel

  let return = Lwt.return

  let bind f p = Lwt.bind p f

  let length _t = 0

  let string _t ~pos:_ ~len:_ = Lwt.return `Eof
end)
