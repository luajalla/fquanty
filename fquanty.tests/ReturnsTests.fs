namespace FQuanty.Tests

open NUnit.Framework
open FsUnit

module ReturnsTests =
    open FQuanty.Performance.Returns
    
    [<Test>]
    let ``simple return``() =
        calculateReturn 102. 100. false |> should approxEqual 0.02
    
    [<Test>]
    let ``cc return``() =
        calculateReturn 102. 100. true |> should approxEqual 0.019802627
        
    [<Test>]
    let ``annualize simple return``() =
        annualizeReturn 0.05 6. false |> should approxEqual 0.3400956406
        
    [<Test>]
    let ``annualize cc return``() =
        annualizeReturn 0.05 6. true |> should approxEqual 0.3
        
    [<Test>]
    let ``calculate rolling n-days simple returns``() =
        let prices = [| 100.; 101.; 101.7; 101.2; 102.; 101.5; 100. |]
        calculateReturns prices 1 false |> Seq.toArray 
        |> should approxEqual [| 0.0100000; 0.0069307; -0.0049164; 0.0079051; -0.0049020; -0.0147783 |]
        calculateReturns prices 3 false |> Seq.toArray 
        |> should approxEqual [| 0.0120000; 0.0099010; -0.0019666; -0.0118577 |]
    
    [<Test>]
    let ``calculate rolling n-days cc returns``() =
        let prices = [| 100.; 101.; 101.7; 101.2; 102.; 101.5; 100. |]
        calculateReturns prices 1 true |> Seq.toArray 
        |> should approxEqual [| 0.0099503; 0.0069068; -0.0049285; 0.0078741; -0.0049140; -0.0148886 |]
        calculateReturns prices 3 true |> Seq.toArray 
        |> should approxEqual [| 0.0119286; 0.0098523; -0.0019685; -0.0119286 |]    
        
    [<Test>]
    let ``5% Value-at-Risk``() =
        VaR 0.05 100000. 0.1 0.07 false |> should approxEqual -1513.97538866
        
    [<Test>]
    let ``5% cc Value-at-Risk``() =
        VaR 0.05 100000. 0.1 0.07 true |> should approxEqual -1502.57239995   
    
    
