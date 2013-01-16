namespace FQuanty.Performance

open FQuanty.Statistics

module Bootstrapping =
    let private random = System.Random()
    
    let randomize (data: _[]) permute =
        let len = data.Length
        if permute then 
            Array.sortBy (fun _ -> random.Next len) data
        else
            Array.init len (fun _ -> data.[random.Next len])

    let bootstrap (data: _[]) f n =
        if data.Length <= 1 || n <= 1 then None
        else
            let samples = Seq.init n (fun _ -> f (randomize data false))
            let original, bootstrapped = f data, Seq.average samples
            let bias = bootstrapped - original
            let error = std samples false
            Some (original, bias, error)
    
    
    let bootstrapMean (data: _[]) n =
        let dataError = std data false / sqrt (float data.Length)
        bootstrap data Seq.average n, dataError
    
    let bootstrapStd (data: _[]) n =
        let dataError = std data false / sqrt (2. * float data.Length)
        bootstrap data (fun x -> std x false) n, dataError
        
        
    open Portfolio
    
    let bootstrapVaR data n p cc =
        let VaR xs = VaRportfolio p xs cc
        bootstrap data VaR n
