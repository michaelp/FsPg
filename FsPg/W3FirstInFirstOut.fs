module W3FirstInFirstOut
(*
In this exercise, we implement a queue with a pair of 
    two lists (front, back) such 
    that front @ List.rev back represents the sequence of elements in the queue.

Write a function is_empty : queue -> bool such that is_empty q is true if and only if q has no element.
Write a function enqueue : int -> queue -> queue such that enqueue x q is the queue as q except that x is at the end of the queue.
Write a function split : int list -> int list * int list such that split l = (front, back) where l = back @ List.rev front and the length of back and front is List.length l / 2 or List.length l / 2 + 1
Write a function dequeue : queue -> int * queue such that dequeue q = (x, q') where x is the front element of the queue q and q' corresponds to remaining elements. This function assumes that q is non empty.

*)

open FSharp.Compatibility.OCaml

type queue = int list * int list
let is_empty : queue -> bool = fun (front,back)->
    match (front,back) with 
    | ([],[]) -> true
    | _ -> false
    
let enqueue x (front, back) =
  (front,x::back)

let dequeue: queue -> int * queue = fun (front,back) ->
    match front with 
    | x::xs -> (x,(xs,back))
    | [] ->
        let rev_back = List.rev back in
        (List.hd rev_back, (List.tl rev_back,[]))

//such that split l = (front, back) where l = back @ List.rev front and the length of back and front is List.length l / 2 or List.length l / 2 + 1
let split : int list -> int list * int list = function
    | [] -> ([],[])
    | l  -> 
        let front_size = List.length l / 2 in
        let rec take x src (front,back) =
            if x <= 0 then (front,src) else take (x-1) (List.tl src) ( (List.hd src) :: front, [] )
        in 
        let (a,b) = take front_size l ([],[]) in
        (List.rev b,List.rev a)
