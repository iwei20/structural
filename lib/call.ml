(* Definitions for calls *)

module Relation = struct
    type t =
    | Less
    | Leq
    | Unknown

    let mul (a : t) (b : t) =
        match (a, b) with
        | (Less, Less) | (Less, Leq) | (Leq, Less) -> Less
        | (Leq, Leq) -> Leq
        | (Unknown, _) | (_, Unknown) -> Unknown

    let add (a : t) (b : t) =
        match (a, b) with
        | (Less, _) | (_, Less) -> Less
        | (Leq, _) | (_, Leq) -> Leq
        | (Unknown, Unknown) -> Unknown
end

module Matrix = struct
    type t = Relation.t array array
    
    let mul (a : t) (b : t) =
        let a_rows = Array.length a in
        let a_cols = Array.length a.(0) in
        let b_rows = Array.length b in
        let b_cols = Array.length b.(0) in 
        assert (a_cols = b_rows);
        Array.init_matrix
            a_rows
            b_cols
            (fun r c ->
                let b_col = Array.init b_rows (fun br -> b.(br).(c)) in
                (* Unknown is the additive identity *)
                Array.map2 Relation.mul a.(r) b_col |> Array.fold_left Relation.add Unknown
            )

end

type fn = {name: string; arity: int}
[@@deriving ord]

module FnOrd = struct
    type t = fn
    let compare = compare_fn
end

module FunctionMap = Map.Make(FnOrd)

module Graph = struct
    type edge = fn * fn * Matrix.t
    type t = edge list

    (* TODO: compose, union *)

    let get_self_edges (graph : t) =
        List.fold_left
            (fun map (f, g, matrix) ->
                if f = g then
                    map |> FunctionMap.update f (Fun.const (Some matrix))
                else
                    map)
            FunctionMap.empty
            graph
end
