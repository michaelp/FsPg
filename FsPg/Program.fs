open FSharp.Compatibility.OCaml

let is_prime n = 
    let top = n |> float_of_int |> sqrt  |> ceil |> int_of_float 
    let rec mapRange i last =
        if i > last then 
           true
        else
            if (n mod i = 0) then false 
            else mapRange (i+1) last
    in 
    match n with
    | 1 ->   false
    | 2 ->   true
    | 3 ->   true
    | _ when n mod 2 = 0 -> false
    | _ -> mapRange 2 top

let test max =
   let rec t i =
        if(i > max) 
            then () 
        else 
            let ip = is_prime i
            //printf "(%d,%b)" i ip
            if ip then printf ",%d" i  else ()
            t (i+1)
   in t 0 
(*
multiple_upto that takes two non-negative 
integers n and r, and that checks whether
n admits at least one divisor 
between 2 and r, returning a boolean.
*)
let multiple_upto n r =
    let rec t i =
        if i > r then false 
        else
           if n mod i = 0 then true else t (i+1) 

    in
    match r with 
    | _ when r < 2 -> false
    | _ when n < 2 -> false
    | _ -> t 2


[<EntryPoint>]
let main argv =
    let res = multiple_upto 77 7
    printfn "%A" argv
    0 // return an integer exit code
