namespace FQuanty

module UniformDistribution =
    let private rand = System.Random()
    
    // density
    let dunif x a b =
        if x < a || x > b || a >= b then 0.
        else 1. / (b - a)

    // distribution function
    let punif x a b =
        if x < a || a >= b then 0.
        else if x > b then 1.
        else (x - a) / (b - a)

    // quantile
    let qunif p a b =
        if p < 0. || p > 1. then nan
        else p * (b - a) + a

    // generate n random values
    let runif n a b =
        if a >= b || n < 1 then Seq.empty
        else 
            Seq.init n (fun _ -> rand.NextDouble() * (b - a) + a)

    // mean
    let munif a b = 0.5 * (a + b)

    // variance
    let vunif a b =
        if a >= b then nan
        else 
            1./12. * (b-a) * (b-a)
