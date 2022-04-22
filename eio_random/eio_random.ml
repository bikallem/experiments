let () =
  (* Eio_luv.run @@ fun env -> *)
  (* Eio_linux.run @@ fun env -> *)
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun _sw ->
  let randon_source = Eio.Stdenv.secure_random env in
  let buf = Cstruct.create 32 in
  let _got = Eio.Flow.read randon_source buf in
  Printf.printf "Random number: %S%!" (Cstruct.to_string buf)
