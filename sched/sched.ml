open Obj.Effect_handlers
open Obj.Effect_handlers.Deep

type _ eff += Fork : (unit -> unit) -> unit eff | Yield : unit eff

let fork f = perform (Fork f)
let yield () = perform Yield

let run main =
  let run_q = Queue.create () in
  let enqueue k = Queue.push k run_q in
  let dequeue () =
    if Queue.is_empty run_q then () else continue (Queue.pop run_q) ()
  in
  let rec spawn f =
    match_with f ()
      { retc= (fun () -> ())
      ; exnc=
          (fun e ->
            print_string @@ Printexc.to_string e ;
            dequeue () )
      ; effc=
          (fun (type a) (e : a eff) ->
            match e with
            | Fork f ->
                Some (fun (k : (a, _) continuation) -> enqueue k ; spawn f)
            | Yield ->
                Some (fun (k : (a, _) continuation) -> enqueue k ; dequeue ())
            | _ -> None ) }
  in
  spawn main
