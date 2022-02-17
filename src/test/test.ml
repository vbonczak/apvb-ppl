
open Array

type 'a support = { values : 'a array;
                    logits : float array;
                    probs : float array;
                  }

type 'a dist = { support : 'a support option;
                 sample : unit -> 'a;
                 logpdf : 'a -> float;
                 exp : (unit -> float) option;
                 var : (unit -> float) option;
               }

let bernoulli p =
  assert(0. <= p && p <= 1.);
  let values = [|0; 1|] in
  let probs = [|1. -. p; p|] in
  let support = {values = values;
                 logits = Array.map log probs;
                 probs = probs}
  in
  let sample () = if Random.float 1. <= p then 1 else 0 in
  let logpdf x = assert(x = 0 || x = 1); support.logits.(x) in
  let exp () = p in
  let var () = p *. (1. -. p) in
  let d = {support = Some support;
           sample = sample;
           logpdf = logpdf;
           exp = Some exp;
           var = Some var;
          } in
  d
;;

let rec pow_int a n = match n with
  |0 -> 1
  |_ -> a * (pow_int a (n-1))
;;

let rec pow_float a n = match n with
  |0 -> 1.
  |_ -> a *. (pow_float a (n-1))
;;

let binomial p n =
  assert(0. <= p && p <= 1. && n >= 0);
  let values = Array.init (pow_int 2 n)
                 (fun i -> Array.init n (fun j -> i mod (pow_int 2 j)))
  in
  (* Each element is an array containing a sequence of ones and zeroes, each element is the binary representation of the current index. *)
  let probs = Array.init (pow_int 2 n)
                (fun i ->
                  let k = Array.fold_left (+) 0 (values.(i)) in
                  (pow_float p k) *. (pow_float (1. -. p) (n-k)))
  in
  let support = {values = values;
                 logits = Array.map log probs;
                 probs = probs;
                } in
  let sample () = Array.init n
                    (fun _ -> if Random.float 1. <= p then 1 else 0)
  in
  let logpdf x = assert(length x = n);
                 Array.iter (fun y -> assert(y = 0 || y = 1)) x;
                 let k = Array.fold_left (+) 0 x in
                 log ((pow_float p k) *. (pow_float (1. -. p) (n-k)))
  in
  let exp () = float_of_int n *. p in
  let var () = float_of_int n *. p *. (1. -. p) in
  let d = {support = Some support;
           sample = sample;
           logpdf = logpdf;
           exp = Some exp;
           var = Some var;
          } in
  d
;;

let uniform a b =
  assert(a < b);
  let sample () = a +. Random.float (b-.a) in
  let logpdf x = if (a <= x && x <= b) then 1. /. (b -. a) else 0. in
  let exp () = (a +. b) /. 2. in
  let var () = (pow_float (b -. a) 2) /. 12. in
  let d = {support = None;
           sample = sample;
           logpdf = logpdf;
           exp = Some exp;
           var = Some var;
          } in
  d
;;

let rec remove x q = match q with (* This function returns q with occurences of x removed and the number of such values removed. *)
  |[] -> [], 0.
  |y::q when y=x -> let l, occs = remove x q in l, occs +. 1.
  |y::q -> let l, occs = remove x q in y::l, occs
;;

let rec regroup l n = match l with (* Returns two lists l1 and l2, l1 gives the elements of l without repetition and l2 gives the corresponding proportion of occurences of each element in l. *)
  |[] -> [],[]
  |x:: q ->
    begin
      let l, occs = remove x q in
      let values, occurences = regroup l n in
      x::values, (occs /. n)::occurences
    end
;;

exception Reject;;

let infer n f = (* Version for finite support and rejection sampling. *)
  assert(n > 0);
  let rec aux n = match n with
  |0 -> []
  |n -> try f()::(aux (n-1)) with Reject -> aux n
  in
  let vals, proportions = regroup (aux n) (float_of_int n) in
  let values = Array.of_list vals in
  let probs = Array.of_list proportions in
  let support = {values = values;
                 logits = Array.map log probs;
                 probs = probs}
  in
  
  let sample () =
    let p = Random.float 1. in (* Assume we split the interval [0,1] into length(prob) intervals, with interval i having length prob.(i). We then generate a random float between 0 and 1 and see which interval it falls in to generate a random value according to prob. *)
    let i = ref 0 in
    let x = ref 0. in
    while p > !x +. support.probs.(!i) do x := !x +. support.probs.(!i); incr i; done;
    support.values.(!i)
  in
  let logpdf x =
    let index, bool = Array.fold_left
                        (fun (i,b) y -> if b || y = x then (i,true) else (i+1,b))
                        (0,false)
                        values
    in
    assert(bool);
    support.logits.(index) in
  let d = {support = Some support;
           sample = sample;
           logpdf = logpdf;
           exp = None;
           var = None;
          } in
  d
  
  

  
                 
               


                 

(*Fin de la partie d'entête*)
(*open Rejection_sampling*)

let funny_bernoulli () = 

let a = 

(bernoulli 0.5).sample() in

let b = 

(bernoulli 0.5).sample() in

let c = 

(bernoulli 0.5).sample() in

(*assume prob (a = 1 || b = 1);*)

a+b+c

let _ =

Format.printf "@.-- Funny Bernoulli, Basic Rejection Sampling --@.";

let d = infer 10000 funny_bernoulli
in

let { values; probs; _ } = Option.get d.support in
Array.iteri (fun i x -> Format.printf "%d %f@." x probs.(i)) values;


