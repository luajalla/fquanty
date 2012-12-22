namespace FQuanty.Tests

open FsUnit
open NUnit.Framework
open FQuanty.Statistics

module StatisticsTests =
    let x = [| 1.; 0.5; 1.5; 2.; 3.; 7.; 0.5; 2.; 2.5 |]
    let y = [| 0.; -1.; 0.5; 1.; 1.; 1.; 2.5; 1.; 0.5 |]
    let z = [| 0.; 0.1; 0.2; 0.; 1.; 0.3; 0.4; 0.5; 0.|]
    
    [<Test>]
    let ``variance n method``() =
        var x true |> should approxEqual 3.5061728
    
    [<Test>]
    let ``variance n-1 method``() =
        var x false |> should approxEqual 3.9444444
    
    [<Test>]
    let ``standard deviation n method``() =
        std x true |> should approxEqual 1.8724777
    
    [<Test>]
    let ``standard deviation n-1 method``() =
        std x false |> should approxEqual 1.9860625
      
    [<Test>]
    let ``cov(x, x) = var(x)``() =
        cov x x true  |> should approxEqual (var x true)
        cov x x false |> should approxEqual (var x false)
        
    [<Test>]
    let ``covariance``() =
        cov x y false |> should approxEqual 0.2881944 
        cov y x false |> should approxEqual 0.2881944 
        
    [<Test>]
    let ``cor(x, x) = 1``() =
        cor x x true  |> should approxEqual 1.
        cor x x false |> should approxEqual 1.
    
    [<Test>]
    let ``cor(x, -x) = -1``() =
        cor x (Array.map (~-) x) true  |> should approxEqual -1.
        cor x (Array.map (~-) x) false |> should approxEqual -1.
        
    [<Test>]
    let ``cor(x, y) = cov(x, y) / (std(x) * std(y))``() =
        let stdx, stdy = std x true, std y true
        cor x y true |> should approxEqual (cov x y true / stdx / stdy)
    
    [<Test>]
    let ``covariance matrix 3x3``() =
        let expected = matrix [[3.9444444; 0.2881944; 0.1368056]
                               [0.2881944; 0.8819444; 0.1243056]
                               [0.1368056; 0.1243056; 0.1069444]]                                        
        covmatrix (matrix [x; y; z]).Transpose false |> should approxEqual expected
        
    [<Test>]
    let ``correlation matrix 3x3``() =
        let expected = matrix [[1.0000000; 0.1545155; 0.2106356]
                               [0.1545155; 1.0000000; 0.4047537]
                               [0.2106356; 0.4047537; 1.0000000]]                                        
        cormatrix (matrix [x; y; z]).Transpose false |> should approxEqual expected
        
    [<Test>]
    let ``downsideDeviation with min = 0``() =
        let xs = [ 0.; 1.; -1.; -2.; 3.; 4.; -1.4 ]
        downsideDeviation xs 0. |> should approxEqual 0.5033223
    

