open Interp
open Lang
open Sys
open Array
open Printf

let input = ref stdin
let output = ref stdout
    
let basename s =
  try String.sub s 0 (String.rindex s '.')
  with Not_found -> s

let usage () =printf "Please enter an input file to compile.\nUsage: %s <INPUT>" executable_name

let () =
  if length argv > 1 then begin
    try
      compile argv.(1)
    with e ->                    
      (*close_in_noerr ic;        *)
      raise e    
  end
  else  usage ();
  ;;  
    
  
    
