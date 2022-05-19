
#load "typingTestsBase.fsx"

open Trulla.Typing
open TypingTestsBase


constr <| fun gen ->
    [
        gen.for' "order" "orders"
        gen.end'
    ]
|> solveProblems



constr <| fun gen ->
    [
        gen.hole "name"
    ]


constr <| fun gen ->
    [
        gen.if' "customer.isActive"
        gen.hole "customer.address.street"
        gen.end'
    ]


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
|> solveProblems


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


constr <| fun gen ->
    [
        gen.for' "a" "rootColl"
        gen.for' "b" "a"
        gen.for' "c" "b"
        gen.hole "c.firstName"
        gen.end'
        gen.end'
        gen.end'
        gen.for' "k" "rootColl"
        gen.for' "l" "k"
        gen.for' "m" "l"
        gen.hole "m.lastName"
        gen.end'
        gen.end'
        gen.end'
    ]
