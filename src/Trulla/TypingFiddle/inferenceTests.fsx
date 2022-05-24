
#load "typingTestsBase.fsx"

open Trulla.Typing
open TypingTestsBase



constr <| fun gen ->
    [
        gen.If "customer.isActive"
        gen.Hole "customer.address.street"
        gen.End
    ]
|> solveProblems
|> buildRecords



// failure
constr <| fun gen ->
    [
        gen.If "customer.isActive"
        gen.Hole "customer"
        gen.End
    ]
|> solveProblems
|> buildRecords




constr <| fun gen ->
    [
        gen.For "order" "orders"
        gen.End
    ]
|> solveProblems
|> buildRecords



constr <| fun gen ->
    [
        gen.Hole "name"
    ]
|> solveProblems
|> buildRecords



constr <| fun gen ->
    [
        gen.For "a" "rootColl"
        gen.For "b" "a"
        gen.For "c" "b"
        gen.Hole "c"
        gen.End
        gen.End
        gen.End
    ]
|> solveProblems
|> buildRecords



constr <| fun gen ->
    [
        gen.For "a" "rootColl"
        gen.For "b" "a"
        gen.For "c" "b"
        gen.Hole "c.name"
        gen.End
        gen.End
        gen.End
    ]
|> solveProblems
|> buildRecords



constr <| fun gen ->
    [
        gen.For "a" "rootColl"
        gen.Hole "a.firstName"
        gen.End
        gen.For "k" "rootColl"
        gen.Hole "k.lastName"
        gen.End
    ]
|> solveProblems



constr <| fun gen ->
    [
        gen.For "a" "rootColl"
        gen.For "b" "a"
        gen.For "c" "b"
        gen.Hole "c.firstName"
        gen.End
        gen.End
        gen.End
        gen.For "k" "rootColl"
        gen.For "l" "k"
        gen.For "m" "l"
        gen.Hole "m.lastName"
        gen.End
        gen.End
        gen.End
    ]
|> solveProblems
