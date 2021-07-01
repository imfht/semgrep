(*
   Rewrite pattern for more efficient matching.

   Right now, this just collapses consecutive '...' into one.
*)

val simplify : Pattern_AST.t -> Pattern_AST.t
