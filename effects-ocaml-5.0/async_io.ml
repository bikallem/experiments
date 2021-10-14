(* Original version : https://github.com/ocaml-multicore/effects-examples/blob/master/aio/aio.ml
 *)
open EffectHandlers
open EffectHandlers.Deep

type _ eff +=
  | Fork : (unit -> unit) -> unit eff
  | Yield : unit eff
  | Accept : Unix.file_descr -> (Unix.file_descr * Unix.sockaddr) eff
  | Recv : Unix.file_descr * bytes * int * int * Unix.msg_flag list -> int eff
  | Send : Unix.file_descr * bytes * int * int * Unix.msg_flag list -> int eff
  | Sleep : float -> unit eff

let fork f = perform (Fork f)
let yield () = perform Yield
let accept fd = perform (Accept fd)
let recv fd buf pos len mode = perform (Recv (fd, buf, pos, len, mode))
let send fd buf pos len mode = perform (Send (fd, buf, pos, len, mode))
let sleep timeout = perform (Sleep timeout)

let poll_read fd =
  let r, _, _ = Unix.select [ fd ] [] [] 0. in
  match r with [] -> false | _ -> true

let poll_write fd =
  let r, _, _ = Unix.select [] [ fd ] [] 0. in
  match r with [] -> false | _ -> true

type read =
  | Accept of (Unix.file_descr * Unix.sockaddr, unit) continuation
  | Recv of bytes * int * int * Unix.msg_flag list * (int, unit) continuation

type write =
  | Send of bytes * int * int * Unix.msg_flag list * (int, unit) continuation

type timeout = Sleep of (unit, unit) continuation

type runnable =
  | Thread : ('a, unit) continuation * 'a -> runnable
  | Read : Unix.file_descr * read -> runnable
  | Write : Unix.file_descr * write -> runnable

type state = {
  run_q : runnable Queue.t;
  read_ht : (Unix.file_descr, read) Hashtbl.t;
  write_ht : (Unix.file_descr, write) Hashtbl.t;
  sleep_ht : (float, timeout) Hashtbl.t;
}

let init () =
  {
    run_q = Queue.create ();
    read_ht = Hashtbl.create 13;
    write_ht = Hashtbl.create 13;
    sleep_ht = Hashtbl.create 13;
  }

let enqueue_thread st k x = Queue.push (Thread (k, x)) st.run_q
let enqueue_read st fd op = Queue.push (Read (fd, op)) st.run_q
let enqueue_write st fd op = Queue.push (Write (fd, op)) st.run_q

let dequeue st =
  match Queue.pop st.run_q with
  | Thread (k, x) -> continue k x
  | Read (fd, Accept k) ->
      let res = Unix.accept fd in
      continue k res
  | Read (fd, Recv (buf, pos, len, mode, k)) ->
      let res = Unix.recv fd buf pos len mode in
      continue k res
  | Write (fd, Send (buf, pos, len, mode, k)) ->
      let res = Unix.send fd buf pos len mode in
      continue k res

let block_accept st fd k = Hashtbl.add st.read_ht fd (Accept k)

let block_recv st fd buf pos len mode k =
  Hashtbl.add st.read_ht fd (Recv (buf, pos, len, mode, k))

let block_send st fd buf pos len mode k =
  Hashtbl.add st.write_ht fd (Send (buf, pos, len, mode, k))

let block_sleep st span k =
  let time = Unix.gettimeofday () +. span in
  Hashtbl.add st.sleep_ht time (Sleep k)

let wakeup st now : bool * float =
  let l, w, n =
    Hashtbl.fold
      (fun t (Sleep k) (l, w, next) ->
        if t <= now then (
          enqueue_thread st k ();
          (t :: l, true, next))
        else if t < next then (l, w, t)
        else (l, w, next))
      st.sleep_ht ([], false, max_float)
  in
  List.iter (fun t -> Hashtbl.remove st.sleep_ht t) l;
  (w, n)

let rec schedule st =
  if Queue.is_empty st.run_q then
    if
      Hashtbl.length st.read_ht = 0
      && Hashtbl.length st.write_ht = 0
      && Hashtbl.length st.sleep_ht = 0
    then () (* We are done. *)
    else
      let now = Unix.gettimeofday () in
      let thrd_has_woken_up, next_wakeup_time = wakeup st now in
      if thrd_has_woken_up then schedule st
      else if next_wakeup_time = max_float then perform_io st (-1.)
      else perform_io st (next_wakeup_time -. now)
  else dequeue st

and perform_io st timeout =
  let rd_fds = Hashtbl.fold (fun fd _ acc -> fd :: acc) st.read_ht [] in
  let wr_fds = Hashtbl.fold (fun fd _ acc -> fd :: acc) st.write_ht [] in
  let rdy_rd_fds, rdy_wr_fds, _ = Unix.select rd_fds wr_fds [] timeout in
  let rec resume ht enqueue = function
    | [] -> ()
    | x :: xs ->
        enqueue st x (Hashtbl.find ht x);
        Hashtbl.remove ht x;
        resume ht enqueue xs
  in
  resume st.read_ht enqueue_read rdy_rd_fds;
  resume st.write_ht enqueue_write rdy_wr_fds;
  if timeout > 0. then ignore (wakeup st (Unix.gettimeofday ())) else ();
  schedule st

let run main =
  let st = init () in
  let rec fork st f =
    match_with f ()
      {
        retc = (fun () -> schedule st);
        exnc =
          (fun exn ->
            print_string (Printexc.to_string exn);
            schedule st);
        effc =
          (fun (type a) (e : a eff) ->
            match e with
            | Yield ->
                Some
                  (fun (k : (a, _) continuation) ->
                    enqueue_thread st k ();
                    schedule st)
            | Fork f ->
                Some
                  (fun (k : (a, _) continuation) ->
                    enqueue_thread st k ();
                    fork st f)
            | Accept fd ->
                Some
                  (fun (k : (a, _) continuation) ->
                    if poll_read fd then
                      let res = Unix.accept fd in
                      continue k res
                    else (
                      block_accept st fd k;
                      schedule st))
            | Recv (fd, buf, pos, len, mode) ->
                Some
                  (fun (k : (a, _) continuation) ->
                    if poll_read fd then
                      let res = Unix.recv fd buf pos len mode in
                      continue k res
                    else (
                      block_recv st fd buf pos len mode k;
                      schedule st))
            | Send (fd, buf, pos, len, mode) ->
                Some
                  (fun (k : (a, _) continuation) ->
                    if poll_write fd then
                      let res = Unix.send fd buf pos len mode in
                      continue k res
                    else (
                      block_send st fd buf pos len mode k;
                      schedule st))
            | Sleep t ->
                Some
                  (fun (k : (a, _) continuation) ->
                    if t <= 0. then continue k ()
                    else (
                      block_sleep st t k;
                      schedule st))
            | _ -> None);
      }
  in
  fork st main
