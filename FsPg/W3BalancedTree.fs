module W3BalancedTree
(*
A binary tree t, of the 'a bt type given in the prelude, is either an empty tree, or the root of a tree with a value and two children subtrees.

Write a function height : 'a bt -> int that computes the height of a tree.
A tree is balanced if, for all internal node n, its two subtrees have the same height. Write a function balanced : 'a bt -> bool that tells if a tree is balanced.
*)
// prelude
type 'a bt =
  | Empty
  | Node of 'a bt * 'a * 'a bt ;;
// prelude - end

let height : 'a bt -> int = fun t ->
    let rec _height t (acc:int)=
        match t with
        | Empty -> acc
        | Node (left,_,right) -> 1 + max (_height left 0)  (_height right 0 )
    in
    _height t 0

let balanced : 'a bt -> bool = fun x ->
    let rec _balanced t =
        match t with
        | Empty -> true
        | Node (left,_,right) -> if height left  = height right  then _balanced left && _balanced right else false
    in
    _balanced x
