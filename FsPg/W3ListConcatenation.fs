module W3ListConcatenation

(*
Concatenating two standard OCaml lists takes a time proportional to the length of the first list. 
In this exercise, we implement a data structure for lists with a constant time concatenation. 

The preludes gives a type 'a clist, which is either a single element of type 'a, 
the concatenation of two 'a clist or an empty 'a clist. 

This representation of a list is not linear: it is a tree-like datastructure since 
the CApp constructor contains two values of type 'a clist. 
The sequence of elements contained in a value of type 'a clist is obtained 
by a depth-first traversal of the tree. 

For instance, the example given in the prelude, of type int clist 
is a valid representation for the sequence [1;2;3;4].

Write a function to_list : 'a clist -> 'a list which computes the 'a list that contains the same elements as the input 'a clist, in the same order.
Write a function of_list : 'a list -> 'a clist which computes the 'a clist that contains the same elements as the input 'a list, in the same order.
Write a function append : 'a clist -> 'a clist -> 'a clist such that:
append CEmpty l = append l CEmpty = l
append l1 l2 = CApp (l1, l2) otherwise
Write a function hd : 'a clist -> 'a option that returns Some x where x is the first element of the input 'a clist, if it is not empty, and returns None otherwise.
Write a function tl : 'a clist -> 'a clist option that returns Some l where l is the input sequence without its first element, if this input sequence is not empty, or returns None otherwise.
*)

// prelude
type 'a clist =
  | CSingle of 'a
  | CApp    of 'a clist * 'a clist
  | CEmpty

let example =
  CApp (CApp (CSingle 1,
              CSingle 2),
        CApp (CSingle 3,
              CApp (CSingle 4, CEmpty)))
// prelude
//Write a function to_list : 'a clist -> 'a list which computes the 'a list that 
// contains the same elements as the input 'a clist, in the same order.
let to_list : 'a clist -> 'a list = fun _cl ->
    let rec _to_list cl res =
        match cl with 
        | CEmpty -> res
        | CSingle a -> a::res
        | CApp (xs,ys) -> res @ _to_list xs [] @ _to_list ys []
    in
    _to_list _cl []

// Write a function of_list : 'a list -> 'a clist which computes the 'a clist that contains the same elements as the input 'a list, in the same order.
let of_list : 'a list -> 'a clist = fun l ->
    let rec _of_list xs res =
        match xs with 
        | [] -> res
        | x::xxs -> _of_list xxs (CApp(CSingle x,res))
    in 
    _of_list (List.rev l) CEmpty

(*
append : 'a clist -> 'a clist -> 'a clist such that:
append CEmpty l = append l CEmpty = l
append l1 l2 = CApp (l1, l2) otherwise
*)
let append : 'a clist -> 'a clist -> 'a clist =
    fun xs ys ->
        match (xs,ys) with
        | (CEmpty,l) -> l
        | (l,CEmpty) -> l
        | (a,b) -> CApp(a,b)


// Write a function hd : 'a clist -> 'a option that returns Some x where x is the first element of the input 'a clist, if it is not empty, and returns None otherwise.
let hd : 'a clist -> 'a option = 
    fun xs->
        let rec _hd (_xs:'a clist) = 
            match _xs with
            | CEmpty -> None
            | CSingle a -> Some a
            | CApp (zs,ws) ->  match (_hd zs) with 
                               | None -> _hd ws
                               | i -> i
        in 
        _hd xs
// Write a function tl : 'a clist -> 'a clist option that
// returns Some l where l is the input sequence without its first element, if this input sequence is not empty, 
// or returns None otherwise.
let tl : 'a clist -> 'a clist option = 
    fun xs->
        let rec _tl ys = match ys with 
            | CEmpty -> None
            | CSingle a -> Some CEmpty
            | CApp (a,CEmpty) ->  _tl a
            | CApp (CEmpty,ws) ->   _tl ws
            | CApp (a,b)-> match (_tl a) with
                           | None -> _tl b
                           | Some x -> Some (CApp(x,b))
        in
            _tl xs
        
