namespace FQuanty.Performance

module Returns =
    let inline calculateReturn p1 p0 contComp =
        if p0 = 0. then 0.
        else if contComp then log p1 - log p0
        else p1 / p0 - 1. 
    
    let inline grossReturn p1 p0 =
        if p0 = 0. then 0. else p1 / p0

    let inline annualizeReturn r n contComp = if contComp then r * n else (1. + r) ** n - 1.

    let calculateReturns prices n contComp =
        if n <= 0 then failwith "invalid number of periods"

        Seq.windowed (n + 1) prices
        |> Seq.map (fun ps -> calculateReturn ps.[ps.Length - 1] ps.[0] contComp)
     
    
    let inline activeReturn (ret, bmkRet) = ret - bmkRet
     
    open FQuanty.NormalDistribution   
        
    // Value-at-Risk 
    let VaR p investment mean sd cc = 
        let q = mean + sd * quantile p
        if cc then (exp q - 1.) * investment
        else q * investment  

