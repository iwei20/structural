(* Definitions for calls *)

module Relation = struct
    type t =
    | Less
    | Leq
    | Unknown
    [@@deriving ord]

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
    [@@deriving ord]
    
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

    let diagonal (a : t) =
        Array.mapi (fun i row -> row.(i)) a
end

module Fn = struct
    type t = {name: string; arity: int}
    [@@deriving ord]
end

module FunctionMap = Map.Make(Fn)

module Edge = struct
    type t = Fn.t * Fn.t * Matrix.t
    [@@deriving ord]
end

module AdjacencyList = Set.Make(Edge)

module Graph = struct
    type t = AdjacencyList.t

    let complete (graph : t) =
        let compose (a : t) (b : t) =
            Seq.map_product
                (fun (g1, h, b) (f, g2, a) -> if g1 = g2 then Some (f, h, (Matrix.mul b a)) else None)
                (AdjacencyList.to_seq a)
                (AdjacencyList.to_seq b)
            |> Seq.filter_map (Fun.id) in
        let result = ref(graph) in
        let next = ref(graph) in
        while not (AdjacencyList.equal !result !next) do
            result := !next;
            next := AdjacencyList.add_seq
                (compose !result graph)
                !result
        done;
        !result

    let get_self_edges (graph : t) =
        AdjacencyList.fold
            (fun (f, g, matrix) map ->
                if f = g then (
                    assert (Array.length matrix = Array.length matrix.(0));
                    map |> FunctionMap.update f (fun l -> match l with
                    | Some l -> Some (matrix::l)
                    | None -> Some [matrix])
                ) else
                    map)
            graph
            FunctionMap.empty
end
