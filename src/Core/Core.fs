namespace Trulla.Core

type Position = { index: int }

module Position =
    let zero = { index = 0 }

type Range = 
    { start: Position; finish: Position }
    static member Zero = { start = Position.zero; finish = Position.zero }

type TrullaError = { ranges: Range list; message: string }

exception TrullaException of TrullaError
