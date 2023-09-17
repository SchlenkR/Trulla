namespace Trulla.Core

type Position = { index: int64; line: int64; column: int64 }
type Range = { start: Position; finish: Position }
type TrullaError = { ranges: Range list; message: string }
exception TrullaException of TrullaError
