namespace FQuanty.Tests

open FsUnit
open NUnit.Framework
open FQuanty

module DistributionsTests =
    open UniformDistribution
    open Statistics
    
    [<Test>]
    let ``uniform distribution density = 1/(b-a)``() =
        dunif 2. 1. 3.5 |> should approxEqual (1./2.5)
        dunif 4. 1. 3.5 |> should approxEqual 0.
        
    [<Test>]
    let ``uniform distribution function = (x-a)/(b-a)``() =
        punif 2. 1. 3.5 |> should approxEqual (1./2.5)
        punif 4. 1. 3.5 |> should approxEqual 1.
        punif 0. 1. 3.5 |> should approxEqual 0.
      
    [<Test>]
    let ``uniform quantile = p*(b-a) + a``() =
        qunif 0.05 1. 3.5 |> should approxEqual (0.05 * 2.5 + 1.)
        qunif  2. 1. 3.5 |> should be NaN
        qunif -1. 1. 3.5 |> should be NaN
            
    [<Test>]
    let ``uniform mean = 0.5*(a+b)``() =
        munif 5. 10. |> should approxEqual 7.5
        runif 1000 5. 10. |> Seq.average |> should (equalWithin 0.2) 7.5

    [<Test>]        
    let ``uniform variance = 1/12*(b-a)^2``() =
        vunif 5. 10. |> should approxEqual 2.08333333
        let xs = runif 1000 5. 10.
        var xs false |> should (equalWithin 0.2) 2.08333333
        
    open Gamma
    
    [<Test>]
    let ``ln of gamma function``() =
        gammaLn 0.  |> should equal None
        gammaLn 0.5 |> Option.get |> should approxEqual 0.572364943
        gammaLn 1.  |> Option.get |> should approxEqual 0.
        gammaLn 7.  |> Option.get |> should approxEqual 6.579251212
        gammaLn 1.  |> Option.get |> should approxEqual 0.
        gammaLn 100.|> Option.get |> should approxEqual 359.13420537
        
    [<Test>]
    let ``incomplete gamma function``() =
        gammaInc 0.5 4.5 |> Option.get |> should approxEqual 0.9973002039137728
        gammaInc 0.1 0.1 |> Option.get |> should approxEqual 0.8275517595703233
        gammaInc 2.0 0.5 |> Option.get |> should approxEqual 0.0902040104206372
        
     
    open NormalDistribution
    
    [<Test>]
    let ``normal distribution density ~N(0.5, 0.1^2)``() =
        dnorm 0.1 0.5 0.1 |> should approxEqual 0.001338302
        dnorm 0.5 0.5 0.1 |> should approxEqual 3.989422804
        dnorm 0.0 0.5 0.1 |> should approxEqual 1.48672e-05
    
    [<Test>]
    let ``normal distribution quantile ~N(0.5, 0.1^2)``() =
        qnorm 0.05 0.5 0.1 true false |> should approxEqual 0.335514637
        qnorm 0.50 0.5 0.1 true false |> should approxEqual 0.500000000
        qnorm 0.95 0.5 0.1 true false |> should approxEqual 0.664485363
        
    [<Test>]
    let ``normal distribution standartized quantile``() =
        quantile 0.05 |> should approxEqual -1.644853627
        quantile 0.50 |> should approxEqual  0.000000000
        quantile 0.72 |> should approxEqual  0.582841507
        quantile 0.95 |> should approxEqual  1.644853627         

    [<Test>]
    let ``normally distributed random numbers``() =
        let rand = System.Random 42
        let xs = rnormWith rand 10000 1.0 0.5
        Seq.average xs |> should (equalWithin 0.01) 1.0
        std xs false   |> should (equalWithin 0.01) 0.5
        
        
        
