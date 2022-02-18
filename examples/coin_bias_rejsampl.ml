open Rejection_sampling

let coin x () =
  let z = sample (uniform 0. 1.) in
  let () = List.iter (observe (bernoulli z)) x in
  z
;;

let _ =
  Random.self_init();
  let dist = infer 1000 (coin [1; 1; 0; 0; 0; 0; 0; 0; 0; 0]) in
  let m = ref 0. in
  let s = Option.get (dist.support) in
  for i=0 to (Array.length (s.values))-1 do
    m := !m +. s.values.(i) *. s.probs.(i)
  done;
  Format.printf "@.-- Coin Bias, Basic Rejection Sampling --@.";
  Format.printf "To get [1; 1; 0; 0; 0; 0; 0; 0; 0; 0], try tossing a coin with bias %f!\n" !m
;;
