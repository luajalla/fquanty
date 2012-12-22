namespace FQuanty.Tests

open FsUnit

[<AutoOpen>]
module FsUnitExt =
    let inline approxEqual x = equalWithin 1e-7 x
