module w2.tetra
open FSharp.Compatibility.OCaml
(*


*)
// x * y
type point2d = {x:int ; y:int}
type tetragon = {lup:point2d; rup:point2d; llp: point2d ; rlp: point2d}

(*
Write a function pairwise_distinct of type tetragon -> bool 
that checks that the points of an input tetragon are pairwise distinct.
*)



