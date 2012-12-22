namespace FQuanty

module Solver =
    let private prec = 1e-8
          
    let newton f guess =
        let rec helper f x count precision =
            let d f x = (f (x + precision) - f (x - precision))/(2. * precision)
            let fx = f x
            let Fx = d f x
            let newX = x - (fx / Fx)
            if abs (newX - x) < precision then Some( newX )
            elif count > 2000 then None
            else helper f newX (count + 1) precision
        helper f guess 0 prec   
        
        
    let bisection f a b =
        let rec helper a b count precision =
            if a = b then failwith (sprintf "(a=b=%f) impossible to start bisection" a) 
                
            let fa = f a
            if abs fa < precision then a // a is the root
            else
                let fb = f b
                if abs fb < precision then b // b is the root
                else
                    let newCount = count + 1
                    
                    if newCount > 2000 then failwith (sprintf "No root found in %i iterations" 2000)
                    if fa * fb > 0. then failwith (sprintf "(%f,%f) don't bracket the root" a b)
                      
                    let midvalue = a + 0.5 * (b - a)
                    let fmid = f midvalue
                        
                    if abs fmid < precision then midvalue // the midvalue is the root
                    elif fa * fmid < 0. then helper a midvalue newCount precision
                    elif fa * fmid > 0. then helper midvalue b newCount precision
                    else failwith "Bisection: It should never get here" 
        helper a b 0 prec 


    let inline swap (fa, fb, a, b) = if abs fa < abs fb then b, a else a, b

    let brent f a' b' =
        match f a', f b', a', b' with
        | fa, fb, _, _ when fa * fb >= 0. -> None // the root is not bracketed
        | fa, fb, a, b | fb, fa, b, a when fa > fb ->
            let rec converge a b c d s bisection =
                let fs = f s
                let b_a = b - a
                if abs b_a < prec || fs = 0. then
                    Some s
                else
                  let fa, fb, fc = f a, f b, f c
                  let s =
                    if fa <> fc && fb <> fc then
                        // inverse quadratic interpolation
                        a*fb*fc/((fa-fb)*(fa-fc)) + b*fa*fc/((fb-fa)*(fb-fc)) + c*fa*fb/((fc-fa)*(fc-fb))
                    else
                        // secant rule
                        b - fb*b_a/(fb - fa)
                  let s_b = abs (s - b)
                  let diff = if bisection then abs (b - c) else abs (c - d)
                  let s, bisection =
                    if s < (3.*a+b)/4. || s > b || s_b >= diff / 2. || diff < prec then
                        (a+b)/2., true
                    else
                        s, false
                  let fs = f s
                  let a', b' = swap (if fa*fs < 0. then fa, fs, a, s else fs, fb, s, b)
                  converge a' b' b c s bisection 
       
            converge a b a b b true  
            
            
    // use combination of bisection and interpolation to find a root
    let findRoot f guess =
        match newton f guess with
        | Some r -> Some r
        | _ -> brent f 0. 3.
    
