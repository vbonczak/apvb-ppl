open Rejection_sampling

let funny_bernoulli () =
  let a = sample (bernoulli 0.5) in
  let b = sample (bernoulli 0.5) in
  let c = sample (bernoulli 0.5) in
  assume (a = 1 || b = 1);
  a+b+c
;;

let _ =
  Random.self_init();
  Format.printf "@.-- Funny Bernoulli, Basic Rejection Sampling --@.";
  let d = infer 10000 funny_bernoulli in
  let { values; probs; _ } = Option.get d.support in
  Array.iteri (fun i x -> Format.printf "%d %f@." x probs.(i)) values;
;;

