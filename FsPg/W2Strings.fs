module W2Strings

open FSharp.Compatibility.OCaml
//open FSharp.Compatibility.OCaml.Array
//open FSharp.Compatibility.OCaml.String


(*
Write a function is_sorted : string array -> bool which 
checks if the values of the input array are sorted in 
strictly increasing order, implying that its elements are unique (use String.compare).
*)
let is_sorted : string array -> bool = fun xs ->
    let f: (bool * string) option -> string -> (bool * string) option = 
        fun state str ->
            match state with
            | None ->  Some (true,str)
            | Some (false, _) -> state
            | Some (true, s)  -> Some ((String.compare s str < 0) ,str)
    in 
    match xs with
        | [||] -> true
        | _ -> match   Array.fold_left f None xs with
               | None -> false
               | Some (r,_) -> r
(*
Using the binary search algorithm, an element can be found very quickly in a sorted array. 
Write a function find : string array -> string -> int such that 
find arr word is the index of the word in the sorted array arr 
if it occurs in arr or -1 if word does not occur in arr. 
The number or array accesses will be counted, to check that you obtain the expected algorithmic complexity.
Beware that you really perform the minimal number of accesses. 
For instance, if your function has to test the contents of a cell twice, 
be sure to put the result of the access in a variable, 
and then perform the tests on that variable.
*)
let find: string array -> string -> int  = fun arr s ->
    let isMatched i = 
        0 = String.compare s (Array.get arr i)
    in
    let rec f (iStart, iEnd)   =
        match (iStart,iEnd) with
        | (x,y) when x = y -> if isMatched iStart then iStart else -1
        | (x,y) when x + 1 = y -> if isMatched iStart then iStart else if isMatched iEnd then iEnd else -1
        | (x,y) -> 
            let pivot = x + (y - x) / 2 in
            let wPivot = Array.get arr pivot in
            let comp = String.compare wPivot s in
            if 0 = comp then pivot else
            if 0 > comp then f (pivot,y) 
            else f (x,pivot) 
    in
        match arr with
        | [||] -> -1
        | _ ->
            let lastIndex = (Array.length arr) - 1 in
            f (0, lastIndex ) 

