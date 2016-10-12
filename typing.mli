exception Error of Syntax.t * (Lexing.position * Lexing.position) * (Type.t * Type.t)
val extenv : Type.t M.t ref
val f : SyntaxPlusPos.t -> Syntax.t

