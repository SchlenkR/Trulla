namespace Trulla.Core

type Range = TheBlunt.Range

type TrullaError =
    { ranges: Range list
      message: string }

exception TrullaException of TrullaError
