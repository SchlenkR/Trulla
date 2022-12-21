[<AutoOpen>]
module Tests.TestHelper

open FsUnit

let inline raiseExn (msg: string) =
    let otype =
        [
            "Xunit.Sdk.XunitException, xunit.assert"
            "NUnit.Framework.AssertionException, nunit.framework"
            "Expecto.AssertException, expecto"
        ]
        |> List.tryPick(System.Type.GetType >> Option.ofObj)
    match otype with
    | None -> failwith msg
    | Some t ->
        let ctor = t.GetConstructor [| typeof<string> |]
        ctor.Invoke [| msg |] :?> exn |> raise


let shouldEqual (a: 'a) (b: 'b) = a |> should equal b
let shouldNotEquals (a: 'a) (b: 'b) = a |> should not' (equal b)
