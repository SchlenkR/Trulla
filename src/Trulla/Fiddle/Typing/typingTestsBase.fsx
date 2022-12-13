
fsi.PrintWidth <- 120
fsi.PrintLength <- 150

#r "nuget: FParsec, 1.1.1"

#load "../../Parsing.fs"
#load "../../Utils.fs"
#load "../../ModelInference.fs"
open Trulla.Internal.Parsing
open Trulla.Internal.ModelInference


let range number =
    let pos number = { index = number; line = 0; column = 0 }
    { start = pos number; finish = pos number }
let pval number t = { value = t; range = range number }
let accessExp segments = MemberToken.createFromSegments segments
let shouldEqual expected actual =
    if expected <> actual 
        then failwith $"Not equal.\nExpected = {expected}\nActual = {actual}"
        else ()

type Gen() =
    let mutable x = -1
    let newNum() = x <- x + 1; x
    let toAcc (path: string) = accessExp [ for x in path.Split [|'.'|] do pval (newNum()) x ]
    member this.For ident path = Token.For (pval (newNum()) ident, toAcc path) |> pval (newNum())
    member this.If path = Token.If (toAcc path) |> pval (newNum())
    member this.Hole path = Token.Hole (toAcc path) |> pval (newNum())
    member this.End = End |> pval (newNum())
let constr x =
    let gen = Gen()
    x gen |> buildTree |> Result.map buildProblems
    




fsi.AddPrinter (fun (o: obj) ->
    match o with
    | :? System.Collections.IEnumerable as enumerable ->
        let singleLine =
            let elemTyp = try enumerable.GetType().GenericTypeArguments[0] with | _ -> typeof<obj>
            [
                typeof<int> 
                typeof<float>
                typeof<string>
                typeof<bool>
            ] 
            |> List.contains elemTyp
        let prefix,elemDelim,ocDelim = 
            if singleLine
            then "", "; ", " "
            else "    ", "\n", "\n"
        let l = enumerable |> Seq.cast<obj> |> Seq.map (fun x -> $"{prefix}{x}") |> Seq.toList
        $"{ocDelim}[{ocDelim}" + (l |> String.concat elemDelim) + $"{ocDelim}]"
    | _ -> null
)

fsi.AddPrinter <| fun (x: Type) ->
    match x with
    | Mono s -> s
    | Poly (n,tp) -> $"%O{n}<%O{tp}>"
    | Field (fn,ft) -> $"""{fn}: %O{ft}"""
    | Var tvar -> string tvar
    | Record tvar -> $"""(RecRef {match tvar with Root -> "ROOT" | TVar tvar -> $"{tvar}"})"""
fsi.AddPrinter <| fun (x: Position) -> $"I{x.index}"
fsi.AddPrinter <| fun (x: Range) -> $"{x.start}-{x.finish}"
//fsi.AddPrinter <| fun (x: PVal) -> $"({this.range}){this.value}"
fsi.AddPrinter <| fun (x: Problem) ->
    let status,cl,cr =
        match x with
        | Unsolved (cl, cr) -> "Unsolved", cl, cr
        | Solved (cl, cr) -> "Solved", cl, cr
    $"{status} %O{cl} : %O{cr}"




//type T = {name: string; age: int}
//fsi.AddPrinter (fun (t: T) -> $"{t.name}({t.age})")

//type U = {person: T; data: string}

//[
//    { person = {name = "Hans"; age = 34}; data = "xxx" }
//    { person = {name = "Ina"; age = 24}; data = "xxx" }
//]

