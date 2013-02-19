namespace FQuanty.Tests

open NUnit.Framework
open FsUnit

module PortfolioTests =
    open FQuanty.Performance.Portfolio
    
    let x = [100.; 99.; 96.; 100.]
    let y = [57. ; 60.; 59.; 60. ]
    let prices = (matrix [x; y]).Transpose
    
    let pfRets = [0.05;0.03;0.06;0.01;0.04;0.04;0.05]
    let bmkRets = [0.04;0.01;0.06;0.02;0.03;0.03;0.04]
    let rf = 0.037 //risk-free
    
    let ws = vector [0.8; 0.2]
    
    open FQuanty.Statistics
    
    [<Test>]
    let ``var of 2 assets portfolio``() =
        let sdx, sdy, covxy = std x false, std y false, cov x y false
        varPortfolio prices ws |> should approxEqual 2.266666667
        sdx*sdx * 0.64 + 2. * covxy * 0.16 + sdy*sdy * 0.04 |> should approxEqual 2.266666667
        
    [<Test>]
    let ``std of 2 assets portfolio``() =
        stdPortfolio prices ws |> should approxEqual 1.505545305
        
    [<Test>]
    let ``global min variance portfolio``() =
        minVariancePortfolio prices |> Option.get |> should approxEqual [| 0.3733333; 0.6266667 |]
        
    [<Test>]
    let ``expected portfolio return``() =
        expectedReturn [| 0.05;0.12;0.6 |] [| 0.3;0.5;0.2 |] |> should approxEqual 0.195 
     
    [<Test>]
    let ``excess returns are diff between pf and bmk``() =
        excessReturns pfRets bmkRets |> should approxEqual [| 0.01;0.02;0.00;-0.01;0.01;0.01;0.01 |]
    
    [<Test>]
    let ``portfolio Value-at-Risk (95%)``() =
        VaRportfolio 0.95 pfRets false |> should approxEqual 0.06686035
         
    [<Test>]
    let ``Sharpe Ratio``() =
        sharpeRatio pfRets rf |> should approxEqual  0.1837117
    
    [<Test>]
    let ``Information Ratio``() =
        informationRatio pfRets bmkRets |> should approxEqual 0.7509393
        
    [<Test>]
    let ``Kelly Ratio``() =
        kellyRatio pfRets rf false |> should approxEqual 11.25
        
    [<Test>]
    let ``portfolio beta``() =  
        beta pfRets bmkRets |> should approxEqual 0.8125
        
    [<Test>]
    let ``Treynor Ratio``() =
        treynorRatio pfRets bmkRets rf |> should approxEqual 0.003692308
        
    [<Test>]
    let ``Sortino Ratio with min = rf``() =
        sortinoRatio pfRets rf |> should approxEqual 0.212132
    
    [<Test>]
    let ``Monte Carlo simulation mean should be close to expected``() =
        let rand = System.Random 42
        let simulation = monteCarloWith rand pfRets 100
        Seq.average simulation |> should (equalWithin 0.01) 0.04
    
    [<Test>]
    let ``Monte Carlo simulation std should be close to expected``() =
        let rand = System.Random 42
        let simulation = monteCarloWith rand pfRets 100
        std simulation false |> should (equalWithin 0.01) 0.0163299               
  