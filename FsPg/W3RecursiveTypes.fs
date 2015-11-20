module W3RecursiveTypes
#nowarn "62"
open FSharp.Compatibility.OCaml
(*
Write a function mem : int -> int list -> bool such that mem x l is true if and only if x occurs in l.
Write a function append : int list -> int list -> int list such that append l1 l2 is the concatenation of l1 and l2.
Write a function combine : int list -> int list -> (int * int) list such that combine l1 l2 is the list of pairs obtained by joining the elements of l1 and l2. This function assumes that l1 and l2 have the same length. For instance, combine [1;2] [3;4] = [(1, 3); (2, 4)].
Write a function assoc : (string * int) list -> string -> int option such that assoc l k = Some x if (k, x) is the first pair of l whose first component is k. If no such pair exists, assoc l k = None.
*)
// Write a function mem : int -> int list -> bool such that mem x l is true if and only if x occurs in l.
let mem : int -> int list -> bool = fun i xs ->
    let rec f ys = 
        match ys with 
        | [] -> false
        | y::ys -> if y = i then true else f ys
    in 
    f xs

let append : int list -> int list -> int list = fun xs ys ->
    let rec rev xs rs =
        match xs with 
        | [] -> rs
        | b::bs -> rev bs (b::rs)
    in
    let rec f zs rs =
        match zs with 
        | z ::zz -> f zz (z::rs)
        | [] -> rs
    in f (rev xs []) ys 

let combine : int list -> int list -> (int * int) list = fun zs ws ->
    let rec f xs ys res =
        match (xs ,ys) with
        | (x::xxs, y::yys) -> f xxs yys ((x,y)::res)
        | _ -> res
    in
     let rec rev xs rs =
        match xs with 
        | [] -> rs
        | b::bs -> rev bs (b::rs)
    in
    f (rev zs []) (rev ws []) []

let assoc: (string * int) list -> string -> int option = fun kvs k ->
    let rec f ys = 
        match ys with 
        | [] -> None
        | (key,value)::ys -> if 0  = String.compare k key then (Some value) else f ys
    in
    f kvs
 