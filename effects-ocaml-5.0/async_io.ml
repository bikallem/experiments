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
      let res = Unis.send fd buf pos len mode in
      continue k res

let block_accept st fd k = Hashtbl.add st.read_ht fd (Accept k)

let block_recv st fd buf pos len mode k =
  Hashtbl.add st.read_ht fd (Recv (buf, pos, len, mode, k))

let block_send st fd buf pos len mode k =
  Hashtbl.add st.write_ht fd (Send (buf, pos, len, mode, k))

let block_sleep st span k =
  let time = Unix.gettimeofday () +. span in
  Hashtbl.add st.sleep_ht time (Sleep k)


