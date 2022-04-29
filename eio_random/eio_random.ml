let () =
  Eio_main.run @@ fun env ->
  Mirage_crypto_rng_eio.run env @@ fun () -> 
  let randon_num = Mirage_crypto_rng.generate 32 in 
  Printf.printf "Random number: %S%!" (Cstruct.to_string randon_num)
