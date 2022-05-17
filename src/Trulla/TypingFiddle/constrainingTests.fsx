
#load "typingTestsBase.fsx"

open Trulla.Typing
open TypingTestsBase

let constr x =
    let gen = newGen()
    x gen
    |> buildTree
    |> collectConstraints
    |> fst



constr <| fun gen ->
    [
        gen.for' "order" "orders"
        gen.end'
    ]
(*
    "$$ROOT$$ : IsRecordDefinition"
    "$$ROOT$$ : HasField { name = "orders"
           typ = Poly ("sequence", TypeId ["T0'order"]) }"
    "T0'order : IsOfType Any"
*)

constr <| fun gen ->
    [
        gen.hole "name"
    ]
(*
    "$$ROOT$$ : IsRecordDefinition"
    "$$ROOT$$ : HasField { name = "name"
            typ = Mono (TypeId ["string"]) }"
*)


constr <| fun gen ->
    [
        gen.for' "a" "rootColl"
        gen.for' "b" "a"
        gen.for' "c" "b"
        gen.hole "c"
        gen.end'
        gen.end'
        gen.end'
    ]
(*
    "$$ROOT$$ : IsRecordDefinition"
    "$$ROOT$$ : HasField { name = "rootColl"
            typ = Poly ("sequence", TypeId ["T0'a"]) }"
    "T0'a : IsOfType Any"
    "T0'a : IsOfType (Poly ("sequence", TypeId ["T1'b"]))"
    "T1'b : IsOfType Any"
    "T1'b : IsOfType (Poly ("sequence", TypeId ["T2'c"]))"
    "T2'c : IsOfType Any"
    "T2'c : IsOfType (Mono (TypeId ["string"]))"
*)


constr <| fun gen ->
    [
        gen.for' "a" "rootColl"
        gen.for' "b" "a"
        gen.for' "c" "b"
        gen.hole "c.name"
        gen.end'
        gen.end'
        gen.end'
    ]
(*
    "$$ROOT$$ : IsRecordDefinition"
    "$$ROOT$$ : HasField { name = "rootColl"
           typ = Poly ("sequence", TypeId ["T0'a"]) }"
    "T0'a : IsOfType Any"
    "T0'a : IsOfType (Poly ("sequence", TypeId ["T1'b"]))"
    "T1'b : IsOfType Any"
    "T1'b : IsOfType (Poly ("sequence", TypeId ["T2'c"]))"
    "T2'c : IsOfType Any"
    "T2'c : IsRecordDefinition"
    "c : HasField { name = "name"
           typ = Mono (TypeId ["string"]) }"
*)

