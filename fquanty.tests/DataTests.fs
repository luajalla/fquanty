namespace FQuanty.Tests

open NUnit.Framework
open FsUnit

module DataTests =
    open FQuanty.DataProviders
    
    let date = System.DateTime(2012, 10, 1)
    
    [<Test>]
    let ``fetch Yahoo symbols for 10/1/2012``() =
        let prices = getYahooSymbols date date "MSFT" Period.Daily
        prices.Length    |> should equal 1
        prices.[0].Open  |> should approxEqual 29.81
        prices.[0].High  |> should approxEqual 29.98
        prices.[0].Low   |> should approxEqual 29.42
        prices.[0].Close |> should approxEqual 29.49
        prices.[0].Volume |> should approxEqual 54042700.
        prices.[0].AdjClose |> Option.get |> should approxEqual 29.25
        
        
    [<Test>]
    let ``fetch Google symbols for 10/1/2012``() =
        let prices = getGoogleSymbols date date "MSFT"
        prices.Length    |> should equal 1
        prices.[0].Open  |> should approxEqual 29.81
        prices.[0].High  |> should approxEqual 29.98
        prices.[0].Low   |> should approxEqual 29.42
        prices.[0].Close |> should approxEqual 29.49
        prices.[0].Volume |> should approxEqual 54042532.
        prices.[0].AdjClose |> should equal None
