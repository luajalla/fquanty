namespace FQuanty

module Statistics =
    (* the methods below use n or n-1 method for std/var calculations *)
    
    let var xs nmethod = 
        match Seq.fold (fun (i, s, sumsq) x -> i + 1, s + x, sumsq + x * x) (0, 0., 0.) xs with
        | 0, _, _ -> 0.
        | 1, _, _ when not nmethod -> 0.
        | n, xsum, xsumsq ->
            // denominator is (n-1) or n
            let d = if nmethod then float n else float n - 1.
            (xsumsq - xsum * xsum / float n) / d
            
    let std xs nmethod = sqrt (var xs nmethod)
    
    let downsideDeviation xs min =
        std (Seq.filter (fun x -> x < min) xs) false  
        
    let cov (xs: #seq<_>) (ys: #seq<_>) nmethod =
        let xy = Seq.zip xs ys
        match Seq.length xy with
        | 0 | 1 when not nmethod -> 0.
        | n ->
            let d = if nmethod then float n else float n - 1.
            let xmean = Seq.sumBy fst xy / float n
            let ymean = Seq.sumBy snd xy / float n
            Seq.sumBy (fun (x, y) -> (x - xmean) * (y - ymean)) xy / d
            
    let cor xs ys nmethod = 
        match std xs nmethod, std ys nmethod with
        | 0., _ | _, 0. -> 0.
        | xstd, ystd -> cov xs ys nmethod / xstd / ystd


    let private initMatrix (items: matrix) nmethod f =
        let n = items.NumCols
        // all variances/covariances
        let x = seq {
                    for i in 0..n - 1 do
                        for j in i..n - 1 do
                            yield f (items.Column i) (items.Column j) nmethod
                } |> Seq.toArray
        Matrix.init items.NumCols items.NumCols (fun i j -> 
            let i', j' = if i <= j then i, j else j, i
            x.[i'*n + j' - (i'+1)*i'/2])   
    
    let covmatrix items nmethod = initMatrix items nmethod cov

    let cormatrix items nmethod = initMatrix items nmethod cor

