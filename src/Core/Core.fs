namespace Trulla.Core

open TheBlunt

type TrullaError =
    { ranges: Range list
      message: string }

exception TrullaException of TrullaError
