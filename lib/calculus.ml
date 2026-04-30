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
    [@@deriving show, ord]
end

module TermMap = Map.Make(Term)

module Dependencies = struct
    type t = (Term.t * Relation.t) TermMap.t
    type l = (Term.t * (Term.t * Relation.t)) list
    [@@deriving show]
end

module FunctionStack = struct
    type t = (string * string list) list
    [@@deriving show]
end

exception Unreachable

let rec relation (deps : Dependencies.t) (a : Term.t) (b : string) =
    let result = match a with
    | Var(a) when a = b -> Relation.Leq (* refl *)
    | Var(a) -> (* trans *)
        begin match TermMap.find_opt (Var(a)) deps with
        | Some(t, Relation.Leq) -> relation deps t b
        | _ -> Relation.Unknown
        end
    | Case(scrut, cases) ->
        (* Invariant: |cases| != 0 *)
        List.fold_left
            (fun acc (y, t) ->
                let case_r = relation
                    (TermMap.add (Var(y)) (scrut, Relation.Leq) deps)
                    t 
                    b
                in
                match (acc, case_r) with
                | (Relation.Less, Relation.Less) -> Less
                | (Relation.Less, Relation.Leq)
                | (Relation.Leq, Relation.Less)
                | (Relation.Leq, Relation.Leq) -> Leq
                | (Relation.Unknown, _)
                | (_, Relation.Unknown) -> Unknown)
            Relation.Less
            cases
    | Proj(_, e) -> relation deps e b
    | App(e_fun, _) -> relation deps e_fun b
    | Unfold(e) ->
        begin match relation deps e b with
        | Relation.Less | Relation.Leq -> Relation.Less
        | Relation.Unknown -> Relation.Unknown
        end
    | _ -> Relation.Unknown
    in
    print_endline
        ("Relation with dependencies " ^
        (Dependencies.show_l (TermMap.to_list deps)) ^
        " between term " ^
        (Term.show a) ^
        " and variable " ^
        b ^
        " is: " ^
        (Relation.show result));
    result

let rec extract (deps : Dependencies.t) (stack : FunctionStack.t) (term : Term.t) =
    let result = match term with
    | Var(x) -> AdjacencyList.empty
    | Inj(_, e) -> extract deps stack e
    | Case(e_scrut, cases) ->
        let calls_scrut = extract deps stack e_scrut in
        List.fold_left
            (fun acc (x, e_case) ->
                AdjacencyList.union
                    acc
                    (extract
                        (TermMap.add (Term.Var(x)) (e_scrut, Relation.Leq) deps)
                        stack
                        e_case))
            calls_scrut
            cases
    | Tuple(components) ->
        List.fold_left
            (fun acc c -> AdjacencyList.union acc (extract deps stack c))
            AdjacencyList.empty
            components
    | Proj(_, e) -> extract deps stack e
    | Lam(_, e_body) -> extract deps stack e_body
    | Rec(f, Lam(args, e_body)) -> extract deps ((f, args)::stack) e_body
    | Rec(_, _) -> raise Unreachable
    | App(e_fun, e_args) ->
        let calls_args =
            List.fold_left
                (fun acc e_arg -> AdjacencyList.union acc (extract deps stack e_arg))
                AdjacencyList.empty
                e_args
        in
        begin match (e_fun, stack) with
        | (Var(g), (f, xs)::tl) ->
            let call_matrix =
                Array.init_matrix
                    (List.length e_args)
                    (List.length xs)
                    (fun i j -> relation deps (List.nth e_args i) (List.nth xs j))
            in
            AdjacencyList.add
                (f, g, call_matrix)
                calls_args
        | (Rec(g, Lam(ys, e_body)), (f, xs)::tl) ->
            let calls_body = extract deps ((g, ys)::stack) e_body in
            let call_matrix =
                Array.init_matrix
                    (List.length e_args)
                    (List.length xs)
                    (fun i j -> relation deps (List.nth e_args i) (List.nth xs j))
            in
            AdjacencyList.add
                (f, g, call_matrix)
                (AdjacencyList.union calls_args calls_body)
        | (_, _) -> AdjacencyList.union calls_args (extract deps stack e_fun)
        end
    | Fold(e) -> extract deps stack e
    | Unfold(e) -> extract deps stack e in
    print_endline (
        "Graph extracted from dependencies " ^
        (Dependencies.show_l (TermMap.to_list deps)) ^
        " and stack " ^
        (FunctionStack.show stack) ^
        " on term " ^
        (Term.show term) ^
        " is " ^
        (AdjListConverted.show (AdjacencyList.to_list result))
        );
    result
