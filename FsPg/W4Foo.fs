﻿module W4Foo
open FSharp.Compatibility.OCaml

(*
Write a function compose : ('a -> 'a) list -> ('a -> 'a) that takes as argument a list l of functions, and that returns the function that is the composition of the functions in l. For instance, compose [f;g;h] x = f (g (h x)). Or with concrete functions, compose [(fun x -> x+1);(fun x -> 3*x);(fun x -> x-1)] 2 = 4.
Write a function fixedpoint : (float -> float) -> float -> float -> float that takes a function f of type float -> float and two floating-point arguments start and delta. The function fixedpoint applies repetitively f to the result of its previous application, starting from start, until it reaches a value y where the difference between y and (f y) is smaller than delta. In that case it returns the value of y. For instance, fixedpoint cos 0. 0.001 yields approximately 0.739 (ref).
*)
// that takes as argument a list l of functions, 
// and that returns the function that is the composition of the functions in l. 
// For instance, compose [f;g;h] x = f (g (h x)). 
// Or with concrete functions, compose [(fun x -> x+1);(fun x -> 3*x);(fun x -> x-1)] 2 = 4.
let compose : ('a -> 'a) list -> ('a -> 'a) = fun l ->
    List.fold_right (fun i state -> (fun x -> i (state x))) l (fun x -> x)
    
// that takes a function f of type float -> float and two floating-point arguments start and delta. 
// The function fixedpoint applies repetitively f to the result of its previous application, starting from start, 
// until it reaches a value y where the difference between y and (f y) is smaller than delta. 
// In that case it returns the value of y. 
//For instance, fixedpoint cos 0. 0.001 yields approximately 0.739 (ref).
let fixedpoint : (float -> float) -> float -> float -> float = fun f start delta ->
    let rec run result = 
        let y = f result in 
        let y' = f y in
        let d = if y' < y then y - y' else y' - y in 
        if d < delta then y else run y
    in run start

let rec equal_on_common2 l1 l2 = match l1,l2 with
  | [],_ -> true
  | _,[] -> true
  | h1::r1,h2::r2 -> h1=h2 && equal_on_common2 r1 r2

let rec equal_on_common: 'a list-> 'a list -> bool = 
  function 
    [] -> fun _ -> true
    | h1::r1 -> 
        function 
        [] -> true
        | h2::r2 when h1 <> h2-> false
        | h2::r2 -> equal_on_common r1 r2 

//  in
//    let ddd = f1 l1 in
//    ddd l1 l2
    
    