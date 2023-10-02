namespace Trulla.Core

type [<Struct>] Range =
    { startIdx: int
      endIdx: int }

type TrullaError = { ranges: Range list; message: string }

exception TrullaException of TrullaError
