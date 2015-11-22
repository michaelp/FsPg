module W3SymbolicOperations
open FSharp.Compatibility.OCaml

// prelude 
type exp =
  | EInt of int
  | EAdd of exp * exp
  | EMul of exp * exp

let example =
  EAdd (EInt 1, EMul (EInt 2, EInt 3))

// prelude 

//Write the expression 2 * 2 + 3 * 3 in a variable my_example
let my_example = EAdd( EMul (EInt 2 , EInt 2), EMul (EInt 3 , EInt 3) )
(*
Write a function eval : exp -> int that computes the value of an arithmetic expression. The evaluation rules are:
If the expression is an integer x, the evaluation is x.
If the expression is lhs + rhs and lhs evaluates to x and rhs evaluates to y, then the evaluation is x + y.
If the expression is lhs * rhs and lhs evaluates to x and rhs evaluates to y, then the evaluation is x * y.
*)
let eval : exp -> int = fun e ->
    let rec evalR (ex:exp) :int = 
        match ex with
            | EInt i -> i
            | EAdd (a,b) -> (evalR a)  + (evalR b)
            | EMul (a,b) -> (evalR a)  * (evalR b)
    in
    evalR e
(*
If an expression is of the form 
    a * b + a * c then a * (b + c) is a factorized equivalent expression. 
    Write a function factorize : exp -> exp that implements this transformation on its input exp 
    if it has the shape a * b + a * c or does nothing otherwise.
*)
let factorize : exp -> exp = fun x ->
    match x with
    | EAdd( EMul ( a , b), EMul (a' , c) ) when a = a' -> EMul (a,EAdd(b,c))
    | _ -> x
        
(*
Write expand : exp -> exp, which turns an expression of the shape a * (b + c) into a * b + a * c.
*)
let expand : exp -> exp = fun x ->
    match x with
    | EMul (a,EAdd(b,c)) -> EAdd( EMul ( a , b), EMul (a , c) ) 
    | _ -> x
(*
Implement a function simplify: exp -> exp which takes an expression e and:
If e is of the shape e * 0 or 0 * e, returns the expression 0.
If e is of the shape e * 1 or 1 * e, returns the expression e.
If e is of the shape e + 0 or 0 + e, returns the expression e.
and does nothing otherwise.
*)
let simplify: exp -> exp = fun x ->
    match x with
    | EMul (_,EInt 0) -> EInt 0
    | EMul (EInt 0,_) -> EInt 0
    | EMul (e,EInt 1) -> e
    | EMul (EInt 1,e) -> e
    | EAdd (EInt 0,e) -> e
    | EAdd (e,EInt 0) -> e
    | _ -> x

