namespace FQuanty

module TimeValueOfMoney =
    type CompoundingFreq =
        | Annually = 1
        | Semiannually = 2 
        | Quarterly = 4
        | Weekly = 52
        | Daily = 365


    (* r   - interest rate per period
       n   - total number of payment periods in an annuity
       pmt - payment made each period
       pv  - present value
       fv  - future value
       due - payments are due at the end/beginning of the period
       g   - growth rate
    *)

    type PaymentsDue =
        | End = 0
        | Beginning = 1
    
    let inline fvAnnuityFactor r n due =
        if r = 0. then float n
        else
            let k = if due = PaymentsDue.End then 1. else 1. + r
            k / r * ((1. + r) ** n - 1.)
        
    let fvAnnuity r n pmt due =
        if pmt = 0. then 0.
        else
            -pmt * fvAnnuityFactor r n due

    let inline pvAnnuityFactor r g n due =
        if r = 0. then float n
        else
            let k = if due = PaymentsDue.End then 1. else 1. + r
            k / (r - g) * (1. - ((1. + g) / (1. + r)) ** n)

    let pvAnnuity r g n pmt due =
        if pmt = 0. then 0.
        else
            -pmt * pvAnnuityFactor r g n due

    let inline pvPerpetuity r g pmt =
        if g > r then failwith "growth rate should be less than interest rate"
        -pmt / (r - g) 

    let fv r n pmt pv due =
        -pv * (1. + r) ** n + fvAnnuity r n pmt due

    let pv r n pmt fv due =
        -fv / (1. + r) ** n + pvAnnuity r 0. n pmt due

    let pmt r n pv fv due =
        - (pv + fv / (1. + r) ** n) / pvAnnuityFactor r 0. n due


    // future value after n years if interest is paid m times per year
    // None is used for continuously compoundining
    let fvm r n pv = function
        | Some m when m > 0 -> -pv * pown (1. + r / float m) (m * n)
        | None -> -pv * exp (r * float n)
        | _ -> failwith "the frequency of compounding should be greater than 0"

    let pvm r n fv = function
        | Some m when m > 0 -> -fv / pown (1. + r / float m) (m * n)
        | None -> -fv / exp (r * float n)
        | _ -> failwith "the frequency of compounding should be greater than 0"

    let inline effectiveAnnualRate r m =
        pown (1. + r / float m) m - 1.

    let inline effectiveAnnualCcRate r m =
        float m * log (1. + r / float m)
        
    let inline private nperFactor rate pmt v = v * rate + pmt
    
    // the number of periods for an investment based on periodic, constant payments and a constant interest rate
    let nper rate pmt pv fv =
        log (nperFactor rate pmt -fv / nperFactor rate pmt pv) / log (1. + rate)
        
    // find periodic rate
    let rate nper pmt presentValue futureValue =
        let f r = pv r nper pmt futureValue PaymentsDue.End - presentValue
        Solver.findRoot f 0.1 
