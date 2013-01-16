#nowarn "25" // incomplete pattern matching

namespace FQuanty.Tests

open NUnit.Framework
open FsUnit

module BootstrappingTests =
    open FQuanty.Performance.Bootstrapping
    
    let xs = [| 0.30407544; 0.70253133;-0.10703893;-0.59218925;-0.01768260; 0.72788002; 0.88902667; 0.04042823;
                0.36947959; 0.94127015; 0.28440701;-0.90375486;-0.40459224; 0.78468116;-0.34486690;-0.68404276;
               -0.32234575;-0.36633226; 0.64982851; 0.17339785; 0.73893639;-0.94022863;-0.44715749;-0.04098060;
               -0.21651845;-0.93660764;-0.60020895; 0.31431471; 0.14326510; 0.68922694 |]
               
    let zero _ = 0.  
          
    [<Test>]
    let ``mean error estimate equals to sd/sqrt len``() =
        let _, dataError = bootstrapMean xs 2
        dataError |> should approxEqual 0.1057796
    
    [<Test>]
    let ``std error estimate equals to sd/sqrt (2*len)``() =
        let _, dataError = bootstrapStd xs 2
        dataError |> should approxEqual 0.07479744
        
    [<Test>]
    let ``bootstrap mean``() =
        let Some(original, bias, error), _ = bootstrapMean xs 1000
        original |> should approxEqual 0.02760673
        bias     |> should (equalWithin 0.01) 0.002279674
        error    |> should (equalWithin 0.005) 0.1024261
        
    [<Test>]
    let ``bootstrap std``() =
        let Some(original, bias, error), _ = bootstrapStd xs 1000
        original |> should approxEqual 0.5793785
        bias     |> should (equalWithin 0.01) -0.01239405
        error    |> should (equalWithin 0.005) 0.05079551
    
    [<Test>]
    let ``bootstrap 5% VaR``() =
        let (Some(original, bias, error)) = bootstrapVaR xs 1000 0.05 true    
        original |> should approxEqual -0.6036217
        bias     |> should (equalWithin 0.01) 0.01175444
        error    |> should (equalWithin 0.005) 0.05724196
                                    
    [<Test>]
    let ``at least 2 samples are needed for bootstrap``() =
        bootstrap [|1.; 2.; 3.|] zero 0 |> should equal None
        bootstrap [|1.; 2.; 3.|] zero 1 |> should equal None
        
    [<Test>]
    let ``at least 2 data items are needed for bootstrap``() =
        bootstrap [||] zero 10 |> should equal None
        bootstrap [|1.|] zero 10 |> should equal None
        
        
        
