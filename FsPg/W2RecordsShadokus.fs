module W2RecordsShadokus
#nowarn "62"
open FSharp.Compatibility.OCaml
(*
On planet Shadokus, a year has 5 months, each month has 4 days, 
each day has 3 hours and each hour has 2 minutes. 
A calendar date is therefore defined as the record type date of the given prelude.

A date is well-formed if its year index is >= 1, its month index is >= 1 and <= 5, 
its day index is >= 1 and <= 4, its hour index is >= 0 and <= 2, and 
its minute index is >= 0 and <= 1. 

The start of year 12 would be:
{ year = 12; month = 1; day = 1; hour = 0; minute = 0 }
The end of year 12 would be:
{ year = 12; month = 5; day = 4; hour = 2; minute = 1 }

Write a function wellformed : date -> bool which checks that the input date is well formed.

On planet Shadokus, the origin of time is the discovery of the Big-Lambda-Machine, a 
magical computer that evaluates the infinite lambda-term of time. 
It is defined by value the_origin_of_time of the given prelude. 
Write a function next : date -> date which computes the date which 
comes one minute after the input date.

In this computer, the time is represented by an integer that counts 
the number of minutes since 1/1/1 0:0 (the origin of time). 
Write a function of_int : int -> date that converts such an integer into a date.
*)

(*
Prelude
*)
type date =
  { year : int; month : int; day : int;
    hour : int; minute : int }

let the_origin_of_time =
  { year = 1; month = 1; day = 1;
    hour = 0; minute = 0 }

(*

let wellformed date =
  "Replace this string with your implementation." ;;

let next date =
  "Replace this string with your implementation." ;;

let of_int minutes =
  "Replace this string with your implementation." ;;

*)

(* ****************************************************** *)
(*
A date is well-formed if its year index is >= 1, its month index is >= 1 and <= 5, 
its day index is >= 1 and <= 4, its hour index is >= 0 and <= 2, and 
its minute index is >= 0 and <= 1. 

The start of year 12 would be:
{ year = 12; month = 1; day = 1; hour = 0; minute = 0 }
The end of year 12 would be:
{ year = 12; month = 5; day = 4; hour = 2; minute = 1 }

Write a function wellformed : date -> bool which checks that the input date is well formed.
*)
let wellformed (d:date) =
    d.year   >= 1   && 
    d.month  >= 1   && d.month  <= 5 &&
    d.day    >= 1   && d.day    <= 4 &&
    d.hour   >= 0   && d.hour   <= 2 &&
    d.minute >= 0   && d.minute <=1

(* Write a function next : date -> date which computes the date which 
comes one minute after the input date. 
*)
let next: date -> date = fun d ->
    let upYear    d = {d with year = d.year + 1} in
    let upMonth   d = if d.month = 5 then upYear {d with month = 1}  else {d with month = d.month + 1} in
    let upDay     d = if d.day = 4 then upMonth {d with day =1 } else {d with day = d.day + 1} in
    let upHour    d  = if d.hour = 2 then upDay {d with hour = 0} else {d with hour = d.hour + 1} 
    in
    if d.minute = 1 then upHour {d with minute = 0} else {d with minute = d.minute + 1}

(*
the time is represented by an integer that counts 
the number of minutes since 1/1/1 0:0 (the origin of time). 
Write a function of_int : int -> date that converts such an integer into a date.

a year has 5 months, each month has 4 days, 
each day has 3 hours and each hour has 2 minutes. 
*)
let of_int:int->date = fun i ->
    let minInHour  = 2 in
    let minInDays  = 3 * minInHour in
    let minInMonth = 4 * minInDays in
    let minInYear  = 5 * minInMonth in
    let def = { year = 1; month = 1; day = 1; hour = 0; minute = 0 } in
    {def with 
        year   = 1 + (i / minInYear);
        month  = 1 + (i / minInMonth) mod 5 ;
        day    = 1 + (i / minInDays) mod 4
        hour   = (i / minInHour) mod 3 ; 
        minute = i mod 2
    }


