include Calculus
include Call
include Termination

let check (expr : Calculus.Term.t) =
    expr
    |> Calculus.extract Calculus.TermMap.empty []
    |> Call.Graph.complete
    |> Call.Graph.get_self_edges
    |> Termination.check