namespace FQuanty

open System
open System.Net
open System.Globalization

module DataProviders =
    
    type Provider =
        | Yahoo
        | Google
        
    type Period =
        | Daily
        | Weekly
        | Monthly
   
    type Price = {
        Date:     DateTime
        Open:     float
        High:     float
        Low:      float
        Close:    float
        Volume:   float
        AdjClose: float option
    } 
    
    let private download (url : Uri) = 
        let req = WebRequest.Create url :?> HttpWebRequest
        use stream = req.GetResponse().GetResponseStream()
        use reader = new IO.StreamReader(stream)
        reader.ReadToEnd()  
 

    let inline tryParse str =
        let mutable res = Unchecked.defaultof<_>    
        let ok = (^a: (static member TryParse: string * byref< ^a > -> bool) (str, &res))
        res    
    
    let private parse (text: string) =
        text.Split '\n'
        |> Seq.skip 1  //skip headers
        |> Seq.map (fun s ->
            match s.Split ',' with
            | [| date; o; h; l; c; v; adj |] -> 
                Some { Date = tryParse date; 
                       Open = tryParse o; High = tryParse h; Low = tryParse l; Close = tryParse c; 
                       Volume = tryParse v;
                       AdjClose = Some (tryParse adj) }
            | [| date; o; h; l; c; v |] -> 
                Some { Date = tryParse date; 
                       Open = tryParse o; High = tryParse h; Low = tryParse l; Close = tryParse c; 
                       Volume = tryParse v;
                       AdjClose = None }
            | _ -> None)
        |> Seq.filter Option.isSome
        |> Seq.map Option.get
        |> Seq.toArray

    let private periodStr (p: Period) = 
        match p with
        | Period.Monthly -> "m"
        | Period.Weekly -> "w"
        | _ -> "d"
        
    let getYahooSymbols (startDate: DateTime) (endDate: DateTime) symbol p =
        Uri (sprintf "http://ichart.finance.yahoo.com/table.csv?s=%s&e=%d&d=%d&f=%d&g=%s&b=%d&a=%d&c=%d&ignore=.csv" 
                symbol endDate.Day (endDate.Month-1) endDate.Year (periodStr p) startDate.Day (startDate.Month-1) startDate.Year)
        |> download
        |> parse
     
    let getGoogleSymbols (startDate: DateTime) (endDate: DateTime) symbol =
        let inline dstr (date: DateTime) = date.ToString("MMM+d,+yyyy", CultureInfo.InvariantCulture)
        Uri (sprintf "http://finance.google.com/finance/historical?q=%s&startdate=%s&enddate=%s&output=csv"
                symbol (dstr startDate) (dstr endDate))
        |> download
        |> parse
        
    let getSymbols startDate endDate symbol = function
        | Provider.Google -> getGoogleSymbols startDate endDate symbol
        | _ -> getYahooSymbols startDate endDate symbol Period.Daily
        
        
