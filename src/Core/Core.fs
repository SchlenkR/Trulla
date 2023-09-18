namespace Trulla.Core

type Position = 
    { index: int64; line: int64; column: int64 }
    static member Zero = { index = 0L; line = 0L; column = 0L }

type Range = 
    { start: Position; finish: Position }
    static member Zero = { start = Position.Zero; finish = Position.Zero }

type TrullaError = { ranges: Range list; message: string }

exception TrullaException of TrullaError
