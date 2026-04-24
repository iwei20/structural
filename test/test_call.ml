open Structural.Call

let print_matrix (a : Matrix.t) =
    a |>
    Array.iter (fun row ->
        row |> Array.iter (fun elt ->
            print_string (match elt with
            | Relation.Less -> "< "
            | Relation.Leq -> "<= "
            | Relation.Unknown -> "? "
            ));
        print_newline ()
    )
    

let a = [|
    [|Relation.Less; Relation.Unknown|];
    [|Relation.Unknown; Relation.Leq|];
|]

let b = [|
    [|Relation.Less|];
    [|Relation.Less|];
|]

let c = [|
    [|Relation.Unknown; Relation.Leq|];
|]

let d = b |> Matrix.mul a |> Matrix.mul c

let e = c |> Matrix.mul d |> Matrix.mul b

let f = e |> Matrix.mul c

let () =
    print_endline "Testing d = cab";
    print_matrix d;
    assert (d = [|
        [|Relation.Less|];
    |]);
    print_endline "Passed";
    print_newline ();

    print_endline "Testing e = bdc";
    print_matrix e;
    assert (e = [|
        [|Relation.Unknown; Relation.Less|];
        [|Relation.Unknown; Relation.Less|];
    |]);
    print_endline "Passed";
    print_newline ();

    print_endline "Testing f = ce";
    print_matrix f;
    assert (f = [|
        [|Relation.Unknown; Relation.Less|];
    |]);
    print_endline "Passed";
    print_newline ();

