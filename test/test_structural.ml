open Structural.Calculus
include Structural.Check

(* Paper example *)
let test_flat_aux () =
    print_endline "Testing flat/aux example from paper";
    let flat = Term.Rec("f", Term.Lam(["ll"], Term.Case(Term.Unfold(Term.Var("ll")), [
        ("_", Term.Fold(Term.Inj(1, Term.Var("()"))));
        ("p", Term.App(Term.Rec("g", Term.Lam(["l"; "ls"], Term.Case(Term.Unfold(Term.Var("l")), [
            ("_", Term.App(Term.Var("f"), [Term.Var("ls")]));
            ("q", Term.Fold(Term.Inj(2, Term.Tuple([
                Term.Proj(1, Var("q"));
                Term.App(Term.Var("g"), [Term.Proj(2, Term.Var("q")); Term.Var("ls")])
            ]))))
        ]))), [Term.Proj(1, Term.Var("p")); Term.Proj(2, Term.Var("p"))]))
    ]))) in
    assert (Structural.Check.check flat);
    print_string "Checker result: ";
    print_endline (Bool.to_string (Structural.Check.check flat));
    print_endline "Passed";
    print_newline ()

let test_inf () =
    print_endline "Testing infinite recursive example";
    let bad = Term.Rec("f", Term.Lam(["x"], Term.App(Term.Var("f"), [Term.Var("x")]))) in
    assert (not (Structural.Check.check bad));
    print_string "Checker result: ";
    print_endline (Bool.to_string (Structural.Check.check bad));
    print_endline "Passed";
    print_newline ()

let test_sum () =
    (* See Remark 3.3 in the paper for why this fails the check *)
    print_endline "Testing edge case sum example";
    let sum = Term.Rec("sum", Term.Lam(["l"], Term.Case(Term.Unfold(Term.Var("l")), [
        ("_", Term.Fold(Term.Inj(1, Term.Var("()")))); (* nil case *)
        ("hdtl", Term.Case(Term.Unfold(Term.Proj(1, Term.Var("hdtl"))), [
            ("n", Term.Fold(Term.Inj(2, Term.App(Term.Var("sum"), [
                Term.Inj(2, Term.Tuple([Term.Var("n"); Term.Proj(2, Term.Var("hdtl"))]))
            ]))));
            ("_", Term.App(Term.Var("sum"), [Term.Proj(2, Term.Var("hdtl"))]))
        ]))
    ]))) in
    assert (not (Structural.Check.check sum));
    print_string "Checker result: ";
    print_endline (Bool.to_string (Structural.Check.check sum));
    print_endline "Passed";
    print_newline ()

let () =
    test_flat_aux ();
    test_inf ();
    test_sum ()