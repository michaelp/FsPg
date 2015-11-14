module w2tetra
#nowarn "62"
open FSharp.Compatibility.OCaml
open FSharp.Compatibility.OCaml.List

(*


*)
// x * y
type  point2d =  int *int
(* {lup point2d; rup:point2d; llp: point2d ; rlp: point2d}*)
type tetragon = point2d * point2d * point2d * point2d 

(*
Write a function pairwise_distinct of type tetragon -> bool 
that checks that the points of an input tetragon are pairwise distinct.
*)
let pairwise_distinct: tetragon -> bool = fun (lup, rup ,llp,rlp) ->
    let rec unique xs = 
        match xs with
            | x::y::ys when x <> y -> unique (List.tl xs)
            | x::y::ys when x = y -> false
            | x::[]    -> true
            | _ -> true
    in
    [lup;llp;rlp;rup] |> List.stable_sort compare |> unique
    

(*
A tetragon is well-formed if the following properties are verified:
The left upper and the left lower points have the lowest x.
Between the two leftmost points, the left upper point has the greatest y.
Between the two rightmost points, the right upper point has the greatest y.
Write a function wellformed of type tetragon -> bool that returns true if and only if the input tetragon is well formed.
*)
let wellformed: tetragon -> bool = 
    fun ((lu_x,lu_y),(ru_x,ru_y),(ll_x,ll_y),(rl_x,rl_y)) ->
        let rule1 = lu_x < ru_x && ll_x < rl_x
        in
        let rule2 = lu_y > ll_y
        in 
        let rule3 = ru_y > rl_y
        in
        rule1 && rule2 && rule3
 
 (*
 A simple way to rotate a tetragon by 90 degrees clockwise with respect to (0, 0)
 is to rotate each of its points by exchanging their abscissa and ordinate and negating the resulting ordinate. 
 Write a function rotate_point of type point2d -> point2d 
 such that rotate_point p is the point p rotated as explained 
 in the previous paragraph.
 *)
let rotate_point(x,y) = (y,-x)

(*
Once rotated, the points of the tetragon may not be in 
the right order: lup may be now llp, rup may be now llp, etc. 
Write a function reorder of type 
point2d * point2d * point2d * point2d -> tetragon 
that takes 4 pairwise distinct points and returns a wellformed tetragon.
*)
let reorder(p1,p2,p3,p4) =
    match [p1;p2;p3;p4] |> List.stable_sort compare with 
    | (x1,y1)::(x2,y2)::(x3,y3)::[(x4,y4)] ->
        let (lup,llp) = if(y1 > y2) then ((x1,y1),(x2,y2)) else ((x2,y2),(x1,y1)) 
        in
        let (rup,rlp) = if(y3 > y4) then ((x3,y3),(x4,y4)) else ((x4,y4),(x3,y3)) 
        in
        (lup,rup,llp,rlp)
    | _ -> (p1,p2,p3,p4)
(*
Write a function rotate_tetragon that takes a well-formed tetragon and returns a well-formed rotated tetragon.
*)
let rotate_tetragon (p1,p2,p3,p4) =
    reorder( rotate_point p1,rotate_point p2,rotate_point p3,rotate_point p4)
