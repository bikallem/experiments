open EffectHandlers
open EffectHandlers.Deep

type _ eff += Fork : (unit -> unit) -> unit eff | Yield : unit eff

let fork : (unit -> unit) -> unit = fun f -> perform (Fork f)
let yield : unit -> unit = fun () -> perform Yield

let run : (unit -> unit) -> unit =
 fun main ->
  let run_q = Queue.create () in
  let enqueue k = Queue.push k run_q in
  let dequeue () =
    if Queue.is_empty run_q then ()
    else continue (Queue.pop run_q) ()
  in
  let rec spawn f =
    match_with f ()
      { retc= (fun () -> dequeue ())
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

let log = Printf.printf

(* The number should be 7. Illustrates that mutable variables don't need to coded 
inside mutexes since only one mutex runs as any one time. *)
let call_count = ref 0

let rec f id depth =
  log "Start (id,depth): %i,%d\n%!" id depth;
  incr call_count;
  if depth > 0 then begin
    log "Fork (id,depth): %i,%d\n%!" (id * 2 + 1) (depth -1);
    fork (fun () -> f (id * 2 + 1) (depth - 1));
    log "Fork (id,depth): %i,%d\n%!" (id * 2 + 2) (depth -1);
    fork (fun () -> f (id * 2 + 2) (depth - 1))
  end else begin
    log "Yield (id,depth): %i,%i\n%!" id depth;
    yield ();
    log "Resume (id,depth): %i,%i\n%!" id depth;
  end;
  log "Finish (id,depth): %i,%i\n%!" id depth

let () = 
  run (fun () -> f 0 2);
  Printf.printf "call count: %d\n%!" !call_count;
