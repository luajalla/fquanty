namespace FQuanty.Performance

open FQuanty.Statistics
open FQuanty.MatrixInv
open Returns


module Portfolio =
    
    let varPortfolio rets (weights: vector) =
        let covm = covmatrix rets false
        weights.Transpose * covm * weights 

    let stdPortfolio rets weights =
        varPortfolio rets weights |> sqrt
                    
    let minVariancePortfolio rets =
        inv (covmatrix rets false)
        |> Option.bind (fun m ->
            let s = Matrix.sum m
            Array.init m.NumRows (fun i -> Seq.sum (m.Row i) / s)
            |> Some)
            
    let expectedReturn (expectedRets: seq<float>) weights =
        Seq.zip expectedRets weights |> Seq.sumBy (fun (e, w) -> e * w)
        
    let excessReturns (pfRets: seq<float>) bmkRets =
        Seq.zip pfRets bmkRets |> Seq.map activeReturn
    
    let beta ra rb = cov ra rb false / var ra false 
    
    
    let inline private divMeanByStd xs = 
        match std xs false with
        | 0. -> 0.
        | s  -> Seq.average xs / s
    
    let inline private avg rets rf = function
        | true -> Seq.averageBy (fun r -> r - rf) rets
        | _ -> Seq.average rets - rf
        
    // Ratios:
                          
    let sharpeRatio pfRets riskFree = 
        Seq.map (fun r -> r - riskFree) pfRets
        |> divMeanByStd
    
    let informationRatio pfRets bmkRets =
        excessReturns pfRets bmkRets
        |> divMeanByStd

   
    let kellyRatio pfRets riskFree halfMethod = 
        match var pfRets false with
        | 0. -> 0.
        | v  -> 
            let k = if halfMethod then 0.5 else 1.
            k * avg pfRets riskFree false / v
    
    let treynorRatio pfRets bmkRets riskFree =
        avg pfRets riskFree true / beta pfRets bmkRets
   
    let sortinoRatio pfRets riskFree =
        avg pfRets riskFree false / downsideDeviation pfRets riskFree
    
    let VaRportfolio p rets cc =
        let m, s = Seq.average rets, std rets false
        VaR p 1. m s cc
        
