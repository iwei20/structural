include Call

module Term = struct
    type t =
    | Var of string
    | Inj of int * t
    | Case of t * ((string * t) list)
    | Tuple of t list
    | Proj of int * t
    | Lam of string list * t
    | Rec of string * t
    | App of t * t list
    | Fold of t
    | Unfold of t
end

module Dependencies = struct
    type t = (string * Relation.t * string) list
end

module FunctionStack = struct
    type t = (string * string list) list 
end

let rec extract (deps : Dependencies.t) (stack : FunctionStack.t) (term : Term.t) =
    match term with
    | Var(x) -> AdjacencyList.empty
    | Inj(_, e) -> extract deps stack e
    | Case(e_scrut, cases) -> AdjacencyList.empty (* TODO *)
    | Tuple(components) ->
        List.fold_left
            (fun acc c -> AdjacencyList.union acc (extract deps stack c))
            AdjacencyList.empty
            components
    | Proj(_, e) -> extract deps stack e
    | Lam(_, e_body) -> extract deps stack e_body
    | Rec(f, e_body) -> AdjacencyList.empty (* TODO *)
    | App(e_fun, e_args) -> AdjacencyList.empty (* TODO *)
    | Fold(e) -> extract deps stack e
    | Unfold(e) -> extract deps stack e
