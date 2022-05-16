
let rollOut elements =
    let makeRes path curr isLast = {| path = path; curr = curr; isLast = isLast |}
    let rec rollOut elements current =
        [
            match elements with
            | [] -> ()
            | x :: (y :: xs as remaining) ->
                yield makeRes current x false
                yield! rollOut remaining (current @ [x])
            | [x] ->
                yield makeRes current x true
        ]
    rollOut elements []

// TODO: UnitTest this
rollOut ([]: int list)
rollOut [1]
rollOut [1;2;3;4;5]
