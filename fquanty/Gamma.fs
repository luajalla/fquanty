namespace FQuanty

open System 
open FQuanty.Statistics

module Gamma =
    // original implementation: http://people.sc.fsu.edu/~jburkardt/c_src/toms291/toms291.html
    // http://people.sc.fsu.edu/~jburkardt/c_src/asa147/asa147.html 
    [<Literal>]
    let lnsqrt2pi = 0.9189385332046727417803297

    let gammaK = [| 0.9999999999995183; 676.5203681218835; -1259.139216722289; 771.3234287757674; -176.6150291498386; 
                    12.50734324009056; -0.1385710331296526;  0.9934937113930748E-05; 0.1659470187408462E-06 |]
    
    // log of gamma function      
    let gammaLn z =
        if z <= 0. then None
        else 
            let v = gammaK 
                    |> Seq.mapi (fun i k -> 
                        if i = 0 then k else k / (z + float i - 1.))
                    |> Seq.sum
            log v + lnsqrt2pi - (z + 6.5) + (z - 0.5) * log (z + 6.5) |> Some 
  
    // incomplete gamma
    let gammaInc a x  =
        if x <= 0. || a <= 0. then None
        else
            let arg = a * log x - (gammaLn (a + 1.0) |> Option.get) - x
            if arg < log 1.0E-37 then None
            else 
                let f = exp arg
                if f = 0. then None
                else 
                    let rec calc a c value =
                        if c < value * 1.E-09 then value
                        else 
                            let a' = a + 1.
                            let c' = c * x / a'
                            calc a' c' (value + c')

                    Some (f * calc a 1. 1.)
                    
    let chi (xs: seq<float>) = 
        let mean = Seq.average xs        
        let res = xs |> Seq.sumBy (fun x -> (x-mean) * (x-mean))
        res / mean
     
    let pValue degreesOfFreedom distance =
        if degreesOfFreedom < 1 || distance < 0. then None
        else if degreesOfFreedom = 8 then exp(-distance / 2.) |> Some
        else
            gammaInc (float degreesOfFreedom / 2.) (distance / 2.)
        
    let chiTestUniform xs =
        pValue (Seq.length xs - 1) (chi xs)
        |> Option.bind (fun p -> Some (1. - p))
 