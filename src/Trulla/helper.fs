module Trulla.Internal.Helper

type ResultBuilder() =
    member this.Bind(m, f) = Result.bind f m
    member this.Return(value) = Ok value
let result = ResultBuilder()
