open! Lwt.Infix

let ( *> ) a b = a >>= fun _ -> b

let to_dest source =
  let buf = Buffer.create 0 in
  let rec loop () =
    Lwt_stream.get source
    >>= function
    | Some c ->
      Buffer.add_char buf c;
      loop ()
    | None -> Lwt.return_unit
  in
  loop () >|= fun () -> Buffer.contents buf

let fill_source text =
  let src, bp = Lwt_stream.create_bounded 20 in
  let rec loop n =
    if n < String.length text then
      bp#push text.[n] >>= fun () -> loop (n + 1)
    else
      Lwt.return_unit
  in
  loop 0
  *>
  (* Comment line above (26) and uncomment the line below (29), it works. Why?
     isn't '*>' and line 29 the same? *)
  (* >>= fun _ -> *)
  (bp#close;
   Lwt.return ())
  >>= fun () -> to_dest src

let () =
  fill_source "hello world" |> Lwt_main.run |> fun s -> Printf.printf "%s\n%!" s
