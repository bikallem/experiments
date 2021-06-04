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

  val return : 'a -> 'a t

  val bind : ('a -> 'b t) -> 'a t -> 'b t

  val map : ('a -> 'b) -> 'a t -> 'b t

  val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t

  val map3 : ('a -> 'b -> 'c -> 'd) -> 'a t -> 'b t -> 'c t -> 'd t

  val map4 :
    ('a -> 'b -> 'c -> 'd -> 'e) -> 'a t -> 'b t -> 'c t -> 'd t -> 'e t

  val parse : input -> 'a t -> ('a, string) result promise

  module Infix : sig
    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

    val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t

    val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t

    val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t

    val ( <$ ) : 'a -> 'b t -> 'a t

    val ( $> ) : 'a t -> 'b -> 'b t

    val ( *> ) : _ t -> 'b t -> 'b t

    val ( <* ) : 'a t -> _ t -> 'a t

    val ( <|> ) : 'a t -> 'a t -> 'a t

    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

    val ( and* ) : 'a t -> 'b t -> ('a * 'b) t

    val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t

    val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
  end

  include module type of Infix

  module Let_syntax : sig
    val return : 'a -> 'a t

    val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t

    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

    module Let_syntax : sig
      val return : 'a -> 'a t

      val map : 'a t -> f:('a -> 'b) -> 'b t

      val bind : 'a t -> f:('a -> 'b t) -> 'b t

      val both : 'a t -> 'b t -> ('a * 'b) t

      val map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t

      val map3 : 'a t -> 'b t -> 'c t -> f:('a -> 'b -> 'c -> 'd) -> 'd t

      val map4 :
        'a t -> 'b t -> 'c t -> 'd t -> f:('a -> 'b -> 'c -> 'd -> 'e) -> 'e t
    end
  end

  (** {2 Char/String parsers} *)

  val peek_char : char t

  val peek_char_opt : char option t

  val peek_string : int -> string t

  val char : char -> char t

  val char_if : (char -> bool) -> char t

  val string : ?case_sensitive:bool -> string -> string t

  val string_of_chars : char list -> string t

  (** {2 Alternate parsers} *)
  val any : 'a t list -> 'a t

  val alt : 'a t -> 'a t -> 'a t

  (** {2 Repetition} *)

  val recur : ('a t -> 'a t) -> 'a t

  val all : 'a t list -> 'a list t

  val skip : ?at_least:int -> ?up_to:int -> _ t -> int t
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

  let return : 'a -> 'a t =
   fun v (_input : Input.t) ~pos ~succ ~fail:_ -> succ ~pos v

  let bind f p input ~pos ~succ ~fail =
    p input ~pos ~succ:(fun ~pos a -> f a input ~pos ~succ ~fail) ~fail

  let map f p input ~pos ~succ ~fail =
    p input ~pos ~succ:(fun ~pos a -> succ ~pos (f a)) ~fail

  module Infix = struct
    let ( >>= ) p f = bind f p

    let ( >>| ) p f = map f p

    let ( <*> ) f q = f >>= fun f' -> map f' q

    let ( <$> ) f p = return f <*> p

    let ( <$ ) v p = (fun _ -> v) <$> p

    let ( $> ) p v = (fun _ -> v) <$> p

    let ( *> ) : _ t -> 'b t -> 'b t = fun p q -> p >>= fun _ -> q

    let ( <* ) : 'a t -> _ t -> 'a t = fun p q -> p >>= fun a -> a <$ q

    let ( <|> ) : 'a t -> 'a t -> 'a t =
     fun p q input ~pos ~succ ~fail ->
      p input ~pos ~succ ~fail:(fun ~pos _s -> q input ~pos ~succ ~fail)

    let both a b = a >>= fun a -> b >>| fun b -> (a, b)

    let ( let* ) = ( >>= )

    let ( and* ) = both

    let ( let+ ) = ( >>| )

    let ( and+ ) = both
  end

  include Infix

  let map2 f p q = return f <*> p <*> q

  let map3 f p q r = return f <*> p <*> q <*> r

  let map4 f p q r s = return f <*> p <*> q <*> r <*> s

  module Let_syntax = struct
    let return = return

    let ( >>| ) = ( >>| )

    let ( >>= ) = ( >>= )

    module Let_syntax = struct
      let return = return

      let map p ~f = map f p

      let bind p ~f = bind f p

      let both = both

      let map2 p q ~f = map2 f p q

      let map3 p q r ~f = map3 f p q r

      let map4 p q r s ~f = map4 f p q r s
    end
  end

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
         | `String _ ->
           fail ~pos (Format.sprintf "pos:%d, n:%d not enough input" pos n)
         | `Eof ->
           fail ~pos (Format.sprintf "pos:%d, n:%d not enough input" pos n))

  (*+++++ Parsers ++++++*)

  let peek_char : char t =
   fun inp ~pos ~succ ~fail ->
    input 1 inp ~pos ~succ:(fun ~pos s -> succ ~pos s.[0]) ~fail

  let peek_char_opt : char option t =
   fun inp ~pos ~succ ~fail:_ ->
    input 1 inp ~pos
      ~succ:(fun ~pos c -> succ ~pos (Some c.[0]))
      ~fail:(fun ~pos _ -> succ ~pos None)

  let peek_string : int -> string t = input

  let char : char -> char t =
   fun c ->
    input 1
    >>= fun s _ ~pos ~succ ~fail ->
    if s.[0] = c then
      succ ~pos:(pos + 1) c
    else
      fail ~pos
        (Format.sprintf "[char] pos: %d, expected %C, got %C" pos c s.[0])

  let char_if f =
    input 1
    >>= fun s _ ~pos ~succ ~fail ->
    let c = s.[0] in
    if f c then
      succ ~pos:(pos + 1) c
    else
      fail ~pos (Format.sprintf "[char_if] pos:%d %C" pos s.[0])

  let string ?(case_sensitive = true) s =
    let len = String.length s in
    input len
    >>= fun s' _ ~pos ~succ ~fail ->
    if case_sensitive && String.equal s s' then
      succ ~pos:(pos + len) s
    else if String.(equal (lowercase_ascii s) (lowercase_ascii s')) then
      succ ~pos:(pos + len) s
    else
      fail ~pos (Format.sprintf "[string] %S" s)

  let string_of_chars chars = return (String.of_seq @@ List.to_seq chars)

  (*++++++ Alternates +++++*)

  let any : 'a t list -> 'a t =
   fun parsers inp ~pos ~succ ~fail ->
    let rec loop = function
      | [] -> fail ~pos (Format.sprintf "[any] all parsers failed")
      | p :: parsers ->
        p inp ~pos
          ~succ:(fun ~pos a -> succ ~pos a)
          ~fail:(fun ~pos:_ _ -> (loop [@tailrec]) parsers)
    in
    loop parsers

  let alt = ( <|> )

  (*+++++ Repetition +++++*)

  let recur f =
    let rec p inp ~pos ~succ ~fail = f p inp ~pos ~succ ~fail in
    p

  let all : 'a t list -> 'a list t =
   fun parsers inp ~pos ~succ ~fail ->
    let items = ref [] in
    let rec loop pos' = function
      | [] -> succ ~pos:pos' (List.rev !items)
      | p :: parsers ->
        p inp ~pos:pos'
          ~succ:(fun ~pos a ->
            items := a :: !items;
            (loop [@tailrec]) pos parsers)
          ~fail:(fun ~pos:pos'' e -> fail ~pos:pos'' e)
    in
    loop pos parsers

  let skip : ?at_least:int -> ?up_to:int -> 'a t -> int t =
   fun ?(at_least = 0) ?up_to p inp ~pos ~succ ~fail ->
    if at_least < 0 then
      invalid_arg "at_least"
    else if Option.is_some up_to && Option.get up_to < 0 then
      invalid_arg "up_to"
    else
      ();

    let up_to = Option.value up_to ~default:(-1) in
    let rec loop pos skipped_count =
      if up_to = -1 || skipped_count < up_to then
        p inp ~pos
          ~succ:(fun ~pos _ -> (loop [@tailcall]) pos (skipped_count + 1))
          ~fail:(fun ~pos _ -> check skipped_count pos)
      else
        check skipped_count pos
    and check skipped_count pos =
      if skipped_count >= at_least then
        succ ~pos skipped_count
      else
        fail ~pos
          (Format.sprintf "[skip] skipped_count:%d at_least:%d" skipped_count
             at_least)
    in
    loop pos 0
end

module String_parser = Make (struct
  type 'a promise = 'a

  type t = string

  let return a = a

  let bind f promise = f promise

  let length t = String.length t

  let string t ~pos ~len =
    if pos + len <= String.length t then
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
