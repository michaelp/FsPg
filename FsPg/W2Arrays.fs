module W2Arrays
#nowarn "62"
open FSharp.Compatibility.OCaml.Array
(*
min : int array -> int that returns the minimal element of a.
*)
let min: int array -> int = fun a ->
    let rec f m xs =
        match xs with
        | [||] -> m
        | _ -> f (min (Array.get xs 0) m) (Array.sub xs 1 (Array.length xs - 1 ))
    in 
        f (Array.get a 0) a

(*that returns the index of the minimal element of a.*)
let min_index : int array -> int  = fun a ->
    let m = min a in 
    let rec f arr i = 
        if m = (Array.get arr i)  then i else f arr (i+1)
    in 
    f a 0
