module Trulla

open FParsec
open Trulla.Internal.Parsing
open Trulla.Internal.Typing

let parseTemplate templateString =
    match run ptemplate templateString with
    | Success (tokenList,_,_) -> Result.Ok tokenList
    | Failure (msg,error,_) ->
        { TrullaError.range = Position.ofFParsec 0L error.Position |> Position.toRange
          message = msg }
        |> List.singleton
        |> Result.Error
    
