module W3Tries
open FSharp.Compatibility.OCaml

    

(*
The data structure called trie is very convenient to represent a dictionary whose keys are strings. 
It is space-efficient way while providing a very fast lookup function. 
See the page on WikiPedia. 
In this exercise, we will implement such a data structure, assuming that we want to associate integers 
to the strings of the dictionary. 
Let us define a trie using two mutually defined types (given in the prelude):

trie which represents a trie, that is a tree whose root may 
contain an integer and whose children are indexed by characters ;
char_to_children which implements the associative data structure whose keys are 
characters and whose values are trie (childrens).

As a trade-off between speed and memory consumption, we choose an associative list to represent 
the association between characters and children. 

The prelude also gives examples of empty trie and of another one that contains the following pairs (key, value): 
[("A", 15); ("to", 7); ("tea", 3);("ted", 4); ("ten", 12); ("i", 11); ("in", 5); ("inn", 9)].

Write a function children_from_char : char_to_children -> char -> trie option such that
children_from_char m c = Some t if (c, t) is the first pair in m with c as a first component ;
children_from_char m c = None if no such pair exists in m.

Write a function update_children : char_to_children -> char -> trie -> char_to_children such that
children_from_char (update_children m c t) c = Some t ;
children_from_char (update_children m c t) c' = children_from_char m c' for c <> c';
If children_from_char m c = Some t then List.length (update_children m c t') = List.length m.

Write a function lookup : trie -> string -> int option such that lookup trie w = Some i if i is the value of the key w in trie and lookup trie w = None if w is not a key of trie. 
To look for a key in a trie, iterate over the characters of the key from left to right. Given the current character c and the current node of the trie n, look for the children n for character c. If such a children exists, continue with that trie and the remainder of the key. If no such children exists, the key is not in the trie. When the characters of the key are entirely consumed, look at the root of the current trie. If there is an integer, this is the value you are looking for. If there is no integer, the key not in the trie.
Write a function insert : trie -> string -> int -> trie such that lookup (insert trie w k) w = Some k and lookup (insert trie w k) w' = lookup trie w' for w <> w'.

*)

(*PRELUDE START*)
type trie = Trie of int option * char_to_children
and char_to_children = (char * trie) list

let empty =
  Trie (None, [])

let example =
  Trie (None,
    [('i', Trie (Some 11,
                     [('n', Trie (Some 5, [('n', Trie (Some 9, []))]))]));
     ('t',
      Trie (None,
        [('e',
          Trie (None,
            [('n', Trie (Some 12, [])); ('d', Trie (Some 4, []));
             ('a', Trie (Some 3, []))]));
         ('o', Trie (Some 7, []))]));
     ('A', Trie (Some 15, []))])

(*PRELUDE END *)

(*
Write a function children_from_char : char_to_children -> char -> trie option such that
children_from_char m c = Some t if (c, t) is the first pair in m with c as a first component ;
children_from_char m c = None if no such pair exists in m.

*)
let children_from_char : char_to_children -> char -> trie option = fun m c ->
    let foo state (_key,_trie) = 
        match state with
        | Some x -> state
        | _   -> if _key = c then Some _trie  else None
    in
    List.fold_left foo None m

(*
Write a function update_children : char_to_children -> char -> trie -> char_to_children such that
children_from_char (update_children m c t) c = Some t ;
children_from_char (update_children m c t) c' = children_from_char m c' for c <> c';
If children_from_char m c = Some t then List.length (update_children m c t') = List.length m.
*)
let update_children : char_to_children -> char -> trie -> char_to_children = 
    fun m c t -> 
        match (children_from_char m c) with
        | None -> (c, t) :: m
        | Some x -> (c,t) :: (List.remove_assoc c m)
            
(*
Write a function lookup : trie -> string -> int option such 
that lookup trie w = Some i 
if i is the value of the key w in trie and lookup trie w = None if w is not a key of trie. 
To look for a key in a trie, iterate over the characters of the key from left to right.
Given the current character c and the current node of the trie n, 
look for the children n for character c. 
If such a children exists, continue with that trie and the remainder of the key. 
If no such children exists, the key is not in the trie. 
When the characters of the key are entirely consumed, look at the root of the current trie. 
If there is an integer, this is the value you are looking for. If there is no integer, the key not in the trie.

*)
let lookup : trie -> string -> int option = fun t s -> 
    let last = if 0 = String.length s then 0 else String.length s - 1 in
    let rec foo (i, t) =
        match (i,t) with
        | (_,Some (Trie (v,m))) when i = last -> children_from_char m (String.get s i)
        | (_,None) -> None
        | (_,Some (Trie (v,m))) -> foo (i + 1, children_from_char m (String.get s i)  )
    in
    if s = "" then 
        match t with 
            Trie (v,m) -> v
        else 
        let b = foo (0,Some t) in 
        match b with
        | None -> None
        | Some (Trie (v,m)) -> v
    
(* such that lookup (insert trie w k) w = Some k and lookup (insert trie w k) w' = lookup trie w' for w <> w'.
*)
(*
let insert : trie -> string -> int -> trie = fun t s v ->
    let last = if 0 = String.length s then 0 else String.length s - 1 in
    let rec string_to_trie index trie

*)