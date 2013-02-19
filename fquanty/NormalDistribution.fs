namespace FQuanty

open System

module NormalDistribution =
    [<Literal>]
    let sqrt2PI = 2.5066282746310005024157652848110452530069867406099383
    
    // helper function                    
    let private dtq log_p lower_tail p = 
        match log_p, lower_tail with
            | true, true -> exp p
            | true, _ -> 1. - exp p
            | _, true -> p
            | _ -> 1. - p

    // density function
    let dnorm x mean sd =
        1./(sqrt2PI * sd) * exp (-(x-mean)*(x-mean)/(2.*sd*sd))
        
    // quantile
    let qnorm p mu sigma lower_tail log_p =
       let p_ = dtq log_p lower_tail p
       let q = p_ - 0.5
       let v = 
           if abs q <= 0.425 then
            let r = 0.180625 - q * q
            q * (((((((r * 2509.0809287301226727 + 33430.575583588128105) * r
                     + 67265.770927008700853) * r + 45921.953931549871457) * r + 13731.693765509461125) * r 
                     +  1971.5909503065514427) * r + 133.14166789178437745) * r + 3.387132872796366608)
                     / (((((((r * 5226.495278852854561 + 28729.085735721942674) * r + 39307.89580009271061) * r 
                        + 21213.794301586595867) * r + 5394.1960214247511077) * r + 687.1870074920579083) * r 
                        + 42.313330701600911252) * r + 1.)
           else 
             let r1 = if q > 0. then dtq log_p (not lower_tail) p else p_
             let t = if log_p && ((lower_tail && q <= 0.) || ((not lower_tail) && q > 0.)) then p else log r1
             let r = sqrt -t
             if r <= 5. then
                let r = r - 1.6
                (((((((r * 7.7454501427834140764e-4 + 0.0227238449892691845833) * r 
                    + 0.24178072517745061177) * r + 1.27045825245236838258) * r + 3.64784832476320460504) * r 
                    + 5.7694972214606914055) * r + 4.6303378461565452959) * r + 1.42343711074968357734)
                    / (((((((r * 1.05075007164441684324e-9 + 5.475938084995344946e-4) * r
                        + 0.0151986665636164571966) * r + 0.14810397642748007459) * r + 0.68976733498510000455) * r 
                        + 1.6763848301838038494) * r + 2.05319162663775882187) * r + 1.)
             else
                let r = r - 5.
                (((((((r * 2.01033439929228813265e-7 + 2.71155556874348757815e-5) * r + 0.0012426609473880784386) * r 
                    + 0.026532189526576123093) * r + 0.29656057182850489123) * r + 1.7848265399172913358) * r 
                    + 5.4637849111641143699) * r + 6.6579046435011037772)
                    / (((((((r * 2.04426310338993978564e-15 + 1.4215117583164458887e-7) * r + 1.8463183175100546818e-5) * r 
                        + 7.868691311456132591e-4) * r + 0.0148753612908506148525)* r + 0.13692988092273580531) * r 
                        + 0.59983220655588793769) * r + 1.)

       mu + sigma * (if q < 0. then -v else v)

    let quantile p = qnorm p 0. 1. true false   
    
    // gaussian random number generator
    let rec private gaussianRand (rand: Random) =
        let v1 = rand.NextDouble() * 2. - 1.
        let v2 = rand.NextDouble() * 2. - 1.        
        let s = v1*v1 + v2*v2
        if s >= 1. then gaussianRand rand
        else v1 * sqrt(-2. * log s / s)    
    
    // normal random number generator
    let internal rnormWith rand n mean sd =
        if n < 1 || sd <= 0. then Seq.empty
        else 
            Seq.init n (fun _ -> mean + gaussianRand rand * sd)
    
    let private rand = System.Random()
    
    let rnorm n mean sd = rnormWith rand n mean sd
    