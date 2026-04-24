module Term = struct
    type t =
    | Var of string
    | Inj of int * t
    | Case of t * ((string * t) list)
    | Tuple of t list
    | Proj of int * t
    | Lam of string * t
    | Rec of string * t
    | App of t * t
    | Fold of t
    | Unfold of t
end