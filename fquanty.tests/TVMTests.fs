namespace FQuanty.Tests

open NUnit.Framework
open FsUnit

module TVMTests =
    open FQuanty.TimeValueOfMoney 

    (* r   - interest rate per period
       n   - total number of payment periods in an annuity
       pmt - payment made each period
       pv  - present value
       fv  - future value
       due - payments are due at the end/beginning of the period
       g   - growth rate
    *)
    
    let r, g, n = 0.05, 0.03, 10.
    let payment, present, future = 1000., 10000., -28866.8388033

    [<Test>]
    let ``pv annuity factor``() =
        pvAnnuityFactor r g n PaymentsDue.End |> should approxEqual 8.747596154
        pvAnnuityFactor r g n PaymentsDue.Beginning |> should approxEqual (8.747596154 * (1. + r))

    [<Test>]
    let ``pv of annuity``() =
        pvAnnuity r g n -payment PaymentsDue.End |> should approxEqual 8747.5961535
        pvAnnuity r g n -payment PaymentsDue.Beginning |> should approxEqual 9184.9759612
        
    [<Test>]
    let ``fv annuity factor``() =
        fvAnnuityFactor r n PaymentsDue.End |> should approxEqual 12.577892536
        fvAnnuityFactor r n PaymentsDue.Beginning |> should approxEqual (12.577892536 * (1. + r))
        
    [<Test>]
    let ``fv of annuity``() = 
        fvAnnuity r n -payment PaymentsDue.End |> should approxEqual 12577.892535549
        fvAnnuity r n -payment PaymentsDue.Beginning |> should approxEqual 13206.787162326
        
    [<Test>]
    let ``pv of perpetuity``() =
        pvPerpetuity r g -payment |> should approxEqual 50000.
        
    [<Test>]
    let ``fv``() =
        fv r n payment present PaymentsDue.End |> should approxEqual -28866.8388033
        
    [<Test>]
    let ``pv``() =
        pv r n payment future PaymentsDue.End |> should approxEqual 10000.
        
    [<Test>]
    let ``payment made each period``() =
        pmt r n present future PaymentsDue.End |> should approxEqual 1000.
        
    [<Test>]
    let ``effective annual rate``() = 
        effectiveAnnualRate r 10 |> should approxEqual 0.051140132
    
    [<Test>]
    let ``effective annual rate - cont compounding``() =
        effectiveAnnualCcRate r 10 |> should approxEqual 0.049875415  
        
    [<Test>]
    let ``number of periods``() =
        nper r payment present future |> should approxEqual 10.
        
    [<Test>]
    let ``periodic rate``() =
        rate n payment present future |> Option.get |> should approxEqual 0.05
    
         
     