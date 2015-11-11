module w2
open FSharp.Compatibility.OCaml

(* 
If you multiply my grand-son age by four, you know how old I am. 
Now, if you exchange the two digits of our ages then you have to 
multiply by three my age to get the age of my grand-son!

Write a function exchange of type int -> int that takes an integer x between 10 and 99 and 
returns an integer which is x whose digits have been exchanged.
 
For instance, exchange 73 = 37.

Define is_valid_answer of type int * int -> bool such that is_valid_answer (grand_father_age, grand_son_age) 
returns true if and only if grand_father_age and grand_son_age 
verify the constraints of the puzzle.

Write a function find : (int * int) -> (int * int) that takes a pair 
(max_grand_father_age, min_grand_son_age) and returns a solution 
(grand_father_age, grand_son_age) to the problem,
 where min_grand_son_age <= grand_son_age < grand_father_age <= max_grand_father_age or (-1, -1) 
 if there was no valid answer in the given range.

*)
let exchange (x:int) :int = 
    let digits = x mod 10 in
    let tens =  (x - digits) / 10 in
    digits * 10 + tens

let is_valid_answer: (int * int) -> bool= 
    fun x->
        match x with
        | (p, s) when (3 * exchange p = exchange s) && (4*s=p) -> true
        | _ -> false
  
let find (limits: int * int ): int * int =
     let rec foo (min,max) = 
        if min = max 
        then 
           (-1,-1) 
        else 
            if is_valid_answer  (max,min) 
            then 
                (max,min)
            else 
                foo (min + 1,max)
     in
     let rec bar (min, max) =
        if min = max 
        then (-1,-1)
        else 
            match (foo (min,max)) with 
            | (-1,-1) -> bar (min, max - 1)
            | (a,b) -> (a,b)
     in
     match limits with
     | (max_f, min_son) when min_son >= max_f    -> (-1,-1)
     | (max_f, min_son) ->  bar(min_son,max_f) 
