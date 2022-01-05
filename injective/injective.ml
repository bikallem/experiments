module type IO = sig
  type +'a t
  type -'a u

  (* val return : 'a -> 'a t *)
end

module A : IO = struct
  type +'a t = 'a Eio.Std.Promise.t

  type -'a u = 'a Eio.Std.Promise.t

  (* let return x = *) 
end

(* module A : sig *)
(*   type !'a t *)

(*   val return : 'a -> 'a t *)
(* end = struct *)
(*   type !'a t = 'a *)

(*   let return x : 'a t = x *)
(* end *)

(* module B : sig *)
(*   type +!'a t *)

(*   val return : 'a -> 'a t *)
(* end = struct *)
(*   type +!'a t = 'a *)

(*   let return x : 'a t = x *)
(* end *)

(* module C : sig *)
(*   type +'a t *)

(*   val return : 'a -> 'a t *)
(* end = struct *)
(*   type +'a t = 'a *)

(*   let return x = x *)
(* end *)

(* module Io_a : IO = struct *)
(*   type +'a t = 'a Lwt.t *)
(*   type 'a u = 'a Lwt.u *)

(*   let return = Lwt.return *)
(* end *)

(* module Io_b : IO = struct *)
(*   type +!'a t = 'a B.t *)

(*   let return = B.return *)
(* end *)

(* module Io_c : IO = struct *)
(*   type +'a t = 'a C.t *)

(*   let return = C.return *)
(* end *)

let () =
  let (_ : string Io_b.t) = Io_b.return "hello" in
  print_string "test"
