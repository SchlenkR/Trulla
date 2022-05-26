
#load "typingTestsBase.fsx"

open Trulla.Internal.Typing
open TypingTestsBase


constr <| fun gen ->
    [
        gen.Hole "customer.field1"
        gen.Hole "customer.field2"
        gen.Hole "customer.field3"
    ]
|> Result.bind solveProblems
|> Result.map buildRecords



// failure: Can't unitfy types Record (TVar 1) and Mono "string"
constr <| fun gen ->
    [
        gen.Hole "name"
        gen.If "customer.isActive"
        gen.Hole "customer"
        gen.End
    ]
|> Result.bind solveProblems
|> Result.map buildRecords



constr <| fun gen ->
    [
        gen.If "customer.isActive"
        gen.Hole "customer.address.street"
        gen.End
    ]
|> Result.bind solveProblems
|> Result.map buildRecords




constr <| fun gen ->
    [
        gen.For "order" "orders"
        gen.End
    ]
|> Result.bind solveProblems
|> Result.map buildRecords



constr <| fun gen ->
    [
        gen.Hole "name"
    ]
|> Result.bind solveProblems
|> Result.map buildRecords



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
|> Result.bind solveProblems
|> Result.map buildRecords



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
|> Result.bind solveProblems
|> Result.map buildRecords



constr <| fun gen ->
    [
        gen.For "a" "rootColl"
        gen.Hole "a.firstName"
        gen.End
        gen.For "k" "rootColl"
        gen.Hole "k.lastName"
        gen.End
    ]
|> Result.bind solveProblems
|> Result.map buildRecords



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
|> Result.bind solveProblems
|> Result.map buildRecords
