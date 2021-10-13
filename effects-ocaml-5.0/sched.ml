open EffectHandlers
open EffectHandlers.Deep

type _ eff += Fork : (unit -> unit) -> unit eff | Yield : unit eff

let fork : (unit -> unit) -> unit = fun f -> perform (Fork f)
let yield : unit -> unit = fun () -> perform Yield

let run : (unit -> unit) -> unit =
 fun main ->
  let run_q = Queue.create () in
  let suspend k = Queue.push k run_q in
  let run_next () =
    if Queue.is_empty run_q then () else continue (Queue.pop run_q) ()
  in
  let rec spawn f =
    match_with f ()
      { retc= (fun () -> run_next ())
      ; exnc=
          (fun e ->
            print_string @@ Printexc.to_string e ;
            run_next () )
      ; effc=
          (fun (type a) (e : a eff) ->
            match e with
            | Fork f ->
                Some (fun (k : (a, _) continuation) -> suspend k ; spawn f)
            | Yield ->
                Some (fun (k : (a, _) continuation) -> suspend k ; run_next ())
            | _ -> None ) }
  in
  spawn main

let log = Printf.printf

let rec f id depth =
  log "Fibre %d: Start depth: %d\n%!" id depth ;
  if depth > 0 then begin
    log "Fibre %d: Fork (id,depth): %i,%d\n%!" id ((id * 2) + 1) (depth - 1) ;
    fork (fun () -> f ((id * 2) + 1) (depth - 1)) ;
    log "Fibre %d: Fork (id,depth): %i,%d\n%!" id ((id * 2) + 2) (depth - 1) ;
    fork (fun () -> f ((id * 2) + 2) (depth - 1))
  end
  else begin
    log "Fibre %d: Yield\n%!" id ;
    yield () ;
    log "Fibre %d: Resume\n%!" id
  end ;
  log "Fibre %d: Finish\n%!" id

let () = run (fun () -> f 0 1)
