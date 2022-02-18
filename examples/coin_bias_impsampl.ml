open Importance_sampling

let coin score obs =
  let z = sample (uniform 0. 1.) in
  let () = List.iter (observe score (bernoulli z)) obs in
  z
;;

let _ =
  Random.self_init();
  let dist = infer 10000 coin [1; 1; 0; 0; 0; 0; 0; 0; 0; 0] in
  let m = ref 0. in
  let s = Option.get (dist.support) in
  for i=0 to (Array.length (s.values))-1 do
    m := !m +. s.values.(i) *. s.probs.(i)
  done;
  Format.printf "@.-- Coin Bias, Basic Importance Sampling --@.";
  Format.printf "To get [1; 1; 0; 0; 0; 0; 0; 0; 0; 0], try tossing a coin with bias %f!\n" !m
;;
