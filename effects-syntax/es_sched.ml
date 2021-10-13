effect Fork  : (unit -> unit) -> unit
effect Yield : unit

let fork f = perform (Fork f)
let yield () = perform Yield

(* A concurrent round-robin scheduler *)
let run main =
  let run_q = Queue.create () in
  let enqueue k = Queue.push k run_q in
  let dequeue () =
    if Queue.is_empty run_q then ()
    else continue (Queue.pop run_q) ()
  in
  let rec spawn f =
    (* Effect handler => instantiates fiber *)
    match f () with
    | () -> dequeue ()
    | exception e ->
        ( print_string (Printexc.to_string e);
          dequeue () )
    | effect Yield k ->
        ( enqueue k; dequeue () )
    | effect (Fork f) k ->
        ( enqueue k; spawn f )
  in
  spawn main

let log = Printf.printf

let rec f id depth =
  log "Start (id,depth): %i,%d\n%!" id depth ;
  if depth > 0 then begin
    log "%d: Fork (id,depth): %i,%d\n%!" id ((id * 2) + 1) (depth - 1) ;
    fork (fun () -> f ((id * 2) + 1) (depth - 1)) ;
    log "%d: Fork (id,depth): %i,%d\n%!" id ((id * 2) + 2) (depth - 1) ;
    fork (fun () -> f ((id * 2) + 2) (depth - 1))
  end
  else begin
    log "%d: Yield\n%!" id ; yield () ; log "%d: Resume\n%!" id
  end ;
  log "%d: Finish\n%!" id

let () = run (fun () -> f 0 1)
