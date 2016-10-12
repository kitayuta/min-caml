type t = (* MinCamlの構文を表現するデータ型 (caml2html: syntax_t) *)
  | Unit
  | Bool of bool
  | Int of int
  | Float of float
  | Not of t
  | Neg of t
  | Add of t * t
  | Sub of t * t
  | FNeg of t
  | FAdd of t * t
  | FSub of t * t
  | FMul of t * t
  | FDiv of t * t
  | Eq of t * t
  | LE of t * t
  | If of t * t * t
  | Let of (Id.t * Type.t) * t * t
  | Var of Id.t
  | LetRec of fundef * t
  | App of t * t list
  | Tuple of t list
  | LetTuple of (Id.t * Type.t) list * t * t
  | Array of t * t
  | Get of t * t
  | Put of t * t * t
and fundef = { name : Id.t * Type.t; args : (Id.t * Type.t) list; body : t }

let rec of_syntax_plus_pos (term, _) =
    match term with
    | SyntaxPlusPos.Unit -> Unit
    | SyntaxPlusPos.Bool b -> Bool b
    | SyntaxPlusPos.Int n -> Int n
    | SyntaxPlusPos.Float f -> Float f
    | SyntaxPlusPos.Not e -> Not (of_syntax_plus_pos e)
    | SyntaxPlusPos.Neg e -> Neg (of_syntax_plus_pos e)
    | SyntaxPlusPos.Add (e1, e2) -> Add (of_syntax_plus_pos e1, of_syntax_plus_pos e2)
    | SyntaxPlusPos.Sub (e1, e2) -> Sub (of_syntax_plus_pos e1, of_syntax_plus_pos e2)
    | SyntaxPlusPos.FNeg e -> FNeg (of_syntax_plus_pos e)
    | SyntaxPlusPos.FAdd (e1, e2) -> FAdd (of_syntax_plus_pos e1, of_syntax_plus_pos e2)
    | SyntaxPlusPos.FSub (e1, e2) -> FSub (of_syntax_plus_pos e1, of_syntax_plus_pos e2)
    | SyntaxPlusPos.FMul (e1, e2) -> FMul (of_syntax_plus_pos e1, of_syntax_plus_pos e2)
    | SyntaxPlusPos.FDiv (e1, e2) -> FDiv (of_syntax_plus_pos e1, of_syntax_plus_pos e2)
    | SyntaxPlusPos.Eq (e1, e2) -> Eq (of_syntax_plus_pos e1, of_syntax_plus_pos e2)
    | SyntaxPlusPos.LE (e1, e2) -> LE (of_syntax_plus_pos e1, of_syntax_plus_pos e2)
    | SyntaxPlusPos.If (e1, e2, e3) -> If (of_syntax_plus_pos e1, of_syntax_plus_pos e2, of_syntax_plus_pos e3)
    | SyntaxPlusPos.Let (h, e1, e2) -> Let (h, of_syntax_plus_pos e1, of_syntax_plus_pos e2)
    | SyntaxPlusPos.Var v -> Var v
    | SyntaxPlusPos.LetRec (f, e) ->
            LetRec ({
                name = f.SyntaxPlusPos.name;
                args = f.SyntaxPlusPos.args;
                body = of_syntax_plus_pos f.SyntaxPlusPos.body;
            }, of_syntax_plus_pos e)
    | SyntaxPlusPos.App (e, es) -> App (of_syntax_plus_pos e, es |>List.map of_syntax_plus_pos)
    | SyntaxPlusPos.Tuple es -> Tuple (es |> List.map of_syntax_plus_pos)
    | SyntaxPlusPos.LetTuple (h, e1, e2) -> LetTuple (h, of_syntax_plus_pos e1, of_syntax_plus_pos e2)
    | SyntaxPlusPos.Array (e1, e2) -> Array (of_syntax_plus_pos e1, of_syntax_plus_pos e2)
    | SyntaxPlusPos.Get (e1, e2) -> Get (of_syntax_plus_pos e1, of_syntax_plus_pos e2)
    | SyntaxPlusPos.Put (e1, e2, e3) -> Put (of_syntax_plus_pos e1, of_syntax_plus_pos e2, of_syntax_plus_pos e3)

