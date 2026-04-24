open Call

module IntSet = Set.Make(Int)

let check (calls : Matrix.t list FunctionMap.t) =
    let check_index (occupied : IntSet.t) (rb : Relation.t array list) (i : int) =
        if IntSet.mem i occupied then
            false
        else
        let (has_one_less, has_no_unknowns) = List.fold_left
            (fun (has_one_less, has_no_unknowns) diag ->
                match diag.(i) with
                | Relation.Less -> (true, has_no_unknowns)
                | Relation.Leq -> (has_one_less, has_no_unknowns)
                | Relation.Unknown -> (has_one_less, false))
            (false, true)
            rb
        in has_one_less && has_no_unknowns
    in
    let rec check_recursion_behavior (occupied : IntSet.t) (rb : Relation.t array list) =
        match rb with
        | [] -> true
        | hd::tl ->
            assert (List.fold_left (fun check diag -> check && (Array.length diag = Array.length hd)) true tl);
            (* Step one: Look for position i where at least one < exists and no ? *)
            match Array.find_index (check_index occupied rb) (Array.mapi (fun i _ -> i) hd) with
            | Some i ->
                (* Step two: Recurse with omitting position i and any recursion behaviors that have < there *)
                let rb_prime = (List.filter (fun diag -> diag.(i) != Relation.Less) rb) in
                let occupied_prime = (IntSet.add i occupied) in
                check_recursion_behavior occupied_prime rb_prime 
            | None -> false
    in
    calls
    |> FunctionMap.map
        (fun calls ->
            calls
            |> List.map Matrix.diagonal
            |> (check_recursion_behavior IntSet.empty)
            )
    |> (fun map -> FunctionMap.fold (Fun.const ( && )) map true)