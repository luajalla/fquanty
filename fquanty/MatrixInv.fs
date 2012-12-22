namespace FQuanty

module MatrixInv = 
    // perform a matrix LU decomposition (lower triangular matrix * upper triangular matrix)
    let LUdecomposition (A: matrix) =
        let inline zero() = Matrix.zero A.NumCols A.NumCols
        // the algorithm applies to square matrices only
        if A.NumCols <> A.NumRows then None
        else
            let L, U = zero(), zero()
            // init the first U's row and L's column
            U.[0..0, *] <- A.[0..0, *]
            L.[*, 0..0] <- Matrix.map (fun ai0 -> ai0 / U.[0,0]) A.[*,0..0] 

            // Uij =  aij - sum_k^(i-1) ukj * lik
            // Lij = (aij - sum_k^(j-1) ukj * lik) / ujj
            for i in 1..A.NumRows-1 do
                L.[i, i] <- 1.
                for j in 1..A.NumCols - 1 do
                    let lrow = L.Row i
                    let ucol = U.Column j
                    if j >= i then
                        U.[i, j] <- A.[i, j] - lrow.[0..i-1] * ucol.[0..i-1] 
                    if j < i then
                        L.[i, j] <- (A.[i, j] - lrow.[0..j-1] * ucol.[0..j-1]) / U.[j, j]
            Some (L, U)

    // solve for lower matrix 
    let private solveL (L: matrix) =
        let xs = Matrix.zero L.NumRows L.NumRows
        for i in 0..L.NumRows-1 do
            xs.[i, i] <- 1. / L.[i, i]
            for j in 0..i - 1 do
                let a = (L.Row i).[0..i - 1]
                let x = (xs.Column j).[0.. i - 1]
                xs.[i, j] <- - a * x / L.[i, i]
        xs
    
    // inverse matrix using LU factorization
    let inv A =
        match LUdecomposition A with
        | Some (L, U) -> 
            let U', L' = (solveL U.Transpose).Transpose, solveL L
            Some (U' * L')
        | _ -> None 
