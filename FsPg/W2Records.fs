module W2Records
#nowarn "62"
open FSharp.Compatibility.OCaml

type point  = { x : float; y : float; z : float }
type dpoint = { dx : float; dy : float; dz : float }
type physical_object = { position : point; velocity : dpoint }

(*
Write a function move : point -> dpoint -> point such that 
move p dp is the point p whose coordinates have been updated according to dp. 
(x is now x +. dx, y is now y +. dy, z is now z +. dz.
*)
let move : point -> dpoint -> point = fun p dp -> 
    {
        x = p.x +. dp.dx;
        y = p.y +. dp.dy;
        z = p.z +. dp.dz;
    }
(*
Write a function next : physical_object -> physical_object such that 
next o is the physical object o at time t + dt. 
The position of next o is the position of o moved according to its velocity vector.
*)
let next : physical_object -> physical_object = fun x ->
    {
        position= move x.position x.velocity ; 
        velocity = x.velocity
    }
(*
Suppose that these objects are spheres whose radius is 1.0. 
Write a function will_collide_soon : physical_object -> physical_object -> bool 
that tells if at the next instant, the two spheres will intersect.
*)
let will_collide_soon : physical_object -> physical_object -> bool = fun a b ->
    let na = (next a).position  in
    let nb = (next b).position  in
    let dx = na.x -. nb.x in
    let dy = na.y -. nb.y in
    let dz = na.z -. nb.z in
    dx *. dx +. dy *. dy +. dz *. dz < 4.0 
