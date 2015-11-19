module W3OptionTypes
(*
Optional values are commonly used in OCaml in the return type of partial functions,
i.e. functions that may fail on some input. The following questions illustrate such situations. 
In the Pervasives module which is loaded automatically, there is a type option with two constructors: 
Some (e) has type t option if e has type t and represents the presence of some value e of type t. 
None has type t option and represents the absence of some value of type t.

Write a function find : string array -> string -> int option such that find a w = Some idx if a.(idx) = w 
and find a w = None if there is no such index.
Sometimes, when a value of type t is missing, a default value should be used. 
Write a function default_int : int option -> int such that: default_int None = 0 and default_int (Some x) = x.
Write a function merge : int option -> int option -> int option such that:
merge None None = None
merge (Some x) None = merge None (Some x) = Some x
merge (Some x) (Some y) = Some (x + y)
*)
#nowarn "62"
open FSharp.Compatibility.OCaml

//such that find a w = Some idx if a.(idx) = w 
// and find a w = None if there is no such index.
let find: string array -> string -> int option = fun arr s ->
    let f state i  =
        match state with
        | (Some i,_)    -> state
        | (None, idx)   -> if 0 = String.compare i s then (Some idx, idx) else (None, idx + 1) 
    in
    let (res,_) = Array.fold_left f  (None, 0) arr
    in 
    res

// Write a function default_int : int option -> int such that: default_int None = 0 and default_int (Some x) = x.
let default_int : int option -> int = function 
    | None -> 0
    | Some i -> i

(*
merge : int option -> int option -> int option such that:
merge None None = None
merge (Some x) None = merge None (Some x) = Some x
merge (Some x) (Some y) = Some (x + y)
*)
let merge : int option -> int option -> int option = fun x y ->
    match (x,y) with 
    | (None, None)     -> None
    | (Some x, None)   -> Some x
    | (None, Some x)   -> Some x
    | (Some x , Some y) -> Some (x + y)
    