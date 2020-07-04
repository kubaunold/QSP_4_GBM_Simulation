module GemetricBrownianMotion

//Script implementing simulation of single stock pracies as values of Gemoetric Brownian Motion with given drift and volatility

module everything =
    open System //pi and e
    //override ToString method
    type test = {V:int} override this.ToString() = sprintf "%d" this.V

    //generates list of n Uniform RVs from interval [0,1]; here it's [0,1) I guess
    let genRandomNumbersNominalInterval (count:int) (seed:int) : float list=
        let rnd = System.Random(seed)
        List.init count (fun _ -> rnd.NextDouble())

    //input: UniformRM need to be from interval (0,1]
    //input: steps MUST BE EVEN!
    //output: NormalRV have mean=0 and standard_deviation=1
    let normalizeRec (uniformList:float list) (n:int) : float list =
        let rec buildNormalList (normalList:float list) =
            if normalList.Length = n then normalList
            else
                let currentNIdOne = normalList.Length
                let currentNIdTwo = currentNIdOne + 1
                let oneU = uniformList.[currentNIdOne]
                let twoU = uniformList.[currentNIdTwo]
                let oneN = sqrt(-2.*Math.Log(oneU, Math.E))*sin(2.*Math.PI*twoU)
                let twoN = sqrt(-2.*Math.Log(oneU, Math.E))*cos(2.*Math.PI*twoU)
                let newUniforms = [oneN; twoN]
                buildNormalList (normalList@newUniforms)
        buildNormalList []

    //function that writes result to a file
    let writeResultToFile (result:float list list) (fileName:string) =
        let path = __SOURCE_DIRECTORY__ + "/" + fileName
        let wr = new System.IO.StreamWriter(path)
        for p in result do
            //printfn "%A" p
            p |> List.map(string) |> String.concat(" ") |> wr.Write
            "\n" |> wr.Write
        wr.Close()

    //writeResultToFile [[15.; 14.; 78.];[15.; 14.; 78.]] "siemka.txt"

        

    for i in 1..5 do
        printfn "%d" i


    let simulateGBM (count:int) (steps:int) (price:float) (drift:float) (vol:float) (years:int) (seed:int) =
        //start counting trajectories
        let rec buildResult currentResult t =
            if t = count+1 then currentResult
            else
                let normalRV = normalizeRec (genRandomNumbersNominalInterval steps t) steps
        
                //build stock prices list
                let rec buildStockPricesList (currentStockPricesList:float list) (steps:int) (normalId:int) : float list =
                    if normalId = steps-1 then currentStockPricesList
                    else
                        let firstExpTerm =  (drift - (vol**2.)/2.) * (float(t)/float(steps))
                        let secondExpTerm =  vol * sqrt(float(t)/float(steps)) * normalRV.[normalId]
                        let newStockPrice = currentStockPricesList.[normalId] * Math.E ** (firstExpTerm + secondExpTerm)
                        buildStockPricesList (currentStockPricesList@[newStockPrice]) steps (normalId+1)                
                let stockPricesList = buildStockPricesList [price] steps 0
                //printfn "StockPricesList: %A" stockPricesList

                let finalStockPrice = stockPricesList.[stockPricesList.Length - 1]
                //calculate historical (realized) volatility
                let rec buildRList (rList:float list) (index:int) =
                    if index = steps-1 then rList
                    else
                        let currentR =  Math.Log((stockPricesList.[index+1])/(stockPricesList.[index]), Math.E)
                        buildRList (rList@[currentR]) (index+1)

                let rList = buildRList [] 0
                let rAvg = List.average rList
                let sumOfSquares : float = List.fold (fun acc elem -> acc  + (elem - rAvg)**2.) 0. rList
                let historicalVolatilitySquared = float(steps)/(float(years)*(float(steps)-1.)) * sumOfSquares
                //prepare final result being tuple: (finalStockPrice, realizedVolatility)
                let newResult = [finalStockPrice; historicalVolatilitySquared]
                buildResult (currentResult@[newResult]) (t+1)
        let result = buildResult [] 1
        writeResultToFile result "output.txt"
        result

    let count = 50
    let steps = 1000 //must be EVEN!
    let price = 4.20
    let drift = 0.3
    let vol = 0.65
    let years = 1
    let seed = 1

    //let uniformRV = genRandomNumbersNominalInterval steps count
    //let normalRV = normalize uniformRV steps

    let r = simulateGBM count steps price drift vol years seed
    r
    //-----------------------------------------------------------------

    let simulateGBMv2 (count:int) (steps:int) (price:float) (drift:float) (vol:float) (years:int) (seed:int) =
        let countPlusOne = count + 1
        //start counting trajectories
        let rec buildResult currentResult t =
            match t with
            | countPlusOne -> printfn "jestem tutaj"
            | _ -> 
                printfn "a jednak wszed�em tutaj po raz %d" t  
                buildResult [] (t+1)  
                
        let result = buildResult [] 1
        //writeResultToFile result "krzychu.txt"
        result

    let r2 = simulateGBM count steps price drift vol years seed
    r2


    let count = 2
    let countPlusOne = count + 1
    let rec buildResult currentResult t =
        match t with
        | xyz -> printfn "jestem tutaj: %d" xyz
        | _ -> 
            printfn "a jednak wszed�em tutaj po raz %d" t  
            buildResult [] (t+1)  
    buildResult [] 1





module helpers =
    open System //for Math.PI
    type test = {V:int} override this.ToString() = sprintf "%d" this.V
    let getRandomBool = System.Random().Next()%2 |> System.Convert.ToBoolean
    //let getRandomNumber = System.Random().Next()

    let genRandomNumbers count =
        let rnd = System.Random()
        List.init count (fun _ -> rnd.Next()%100)
    let l = genRandomNumbers 10
    printfn "%A" l

    //generates list of n Uniform RVs from interval [0,1]; here it's [0,1) I guess
    let genRandomNumbersNominalInterval count (seed:int) : float list=
        let rnd = System.Random(seed)
        List.init count (fun _ -> rnd.NextDouble())
    let ll = genRandomNumbersNominalInterval 7 20

    //input: UniformRM need to be from interval (0,1]
    //output: NormalRV have mean=0 and standard_deviation=1
    let normalize (uniformList:float list) (n:int) : float list =
        let mutable result = []
        for x in 0..2..(n-1) do
            let oneU = uniformList.[x]
            let twoU = uniformList.[x+1]
            let oneN = sqrt(-2.*Math.Log(oneU, Math.E))*sin(2.*Math.PI*twoU)
            let twoN = sqrt(-2.*Math.Log(oneU, Math.E))*cos(2.*Math.PI*twoU)
            let tempList = [oneN; twoN]
            result <- result @ tempList
        result

module GBM = 
    open helpers
    open System // for Math.E

    let count = 7
    let steps = 1000
    let price = 5.
    let drift = 0.4
    let vol = 0.
    let years = 4
    let seed = 5

    let rec simulateGBM (count:int) (steps:int) (price:float) (drift:float) (vol:float) (years:int) (seed:int) =
        let wr = new System.IO.StreamWriter(__SOURCE_DIRECTORY__ + "/output.txt")
        let mutable stockPrice = [price]    //let zero index be S_0 price
        for c in 1..(count+7) do
            //simulate stock prices
            stockPrice <- [price]
            //prepare Random Variables
            let uniformRV = genRandomNumbersNominalInterval steps c
            printfn "%d" c
            let normalRV = normalize uniformRV steps
            let mutable R = []
            for i in 0..(steps-1) do
                let nextStockPrice = (stockPrice.[i]) * Math.E ** (drift - ((vol**2.)/2.)*(float(years)/float(steps)) + vol * sqrt(float(years)/float(steps)) * normalRV.[i])
                stockPrice <- stockPrice @ [nextStockPrice]

            //calculate historical (realized) volatility
            for i in 0..(steps-1) do
                let nextR = Math.Log((stockPrice.[i+1])/(stockPrice.[i]), Math.E)
                R <- R @ [nextR]

            let RAvg = List.average R
            let sumOfSquares : float = List.fold (fun acc elem -> acc  + (elem - RAvg)**2.) 0. R
            R <- []
            let historicalVolatilitySquared = float(steps)/(float(years)*(float(steps)-1.)) * sumOfSquares

            let result = [stockPrice.[stockPrice.Length - 1]; historicalVolatilitySquared]
            result |> List.map(string) |> String.concat(" ") |> wr.Write
            "\n" |> wr.Write
            stockPrice <- [price]
            printfn "%A" result
        wr.Close()        
    
    simulateGBM count steps price drift vol years seed

module writeToCsv =
    open helpers

    let drawUniformRV (n: int) =
        //let list = [for i in 0..(n-1) -> System.Random().Next()%100]
        let res = genRandomNumbers n
        let res2 = genRandomNumbers n
        let wr = new System.IO.StreamWriter(__SOURCE_DIRECTORY__ + "/uniformRV.csv")
        res |> List.map(string) |> String.concat("\n") |> wr.Write
        //";" |> wr.Write
        //res2 |> List.map(string) |> String.concat(",") |> wr.Write
        wr.Close()
    drawUniformRV 1000

    //it has to be even number!
    let drawStandardNormalRV (n: int) =
        let uniformList = genRandomNumbersNominalInterval 5 n
        let normalList = normalize uniformList n
        let wr = new System.IO.StreamWriter(__SOURCE_DIRECTORY__ + "/normalRV.csv")
        normalList |> List.map(string) |> String.concat("\n") |> wr.Write
        wr.Close()
    drawStandardNormalRV 1000

module csv =
    type test = { G:double; P:double; GG:double; PP:double } override this.ToString() = sprintf "%f;%f;%f;%f\n" this.G this.P this.GG this.PP

    let G_0  =  [|(0.0)..(10.0)|]
    let Un0  =  [|(1.0)..(11.0)|]
    let P0   =  [|(2.0)..(12.0)|]
    let G0   =  [|(3.0)..(13.0)|]
    let PP0  =  [|(4.0)..(14.0)|]

    let table = [for x in 0..(Un0.Length - 1) -> 
            let b = Un0.[x] 
            if b=0.0 then {G=0.0; P=0.0; GG=0.0; PP=0.0}
            else {G=G_0.[x]/b; P=P0.[x]/b; GG=G0.[x]/b; PP=PP0.[x]/b}]

    let wr = new System.IO.StreamWriter(__SOURCE_DIRECTORY__ + "/results.csv")

    table |> List.map(string) |> String.concat("") |> wr.Write
    wr.Close()

    let printSourceLocation() =
        printfn "Line: %s" __LINE__
        printfn "Source Directory: %s" __SOURCE_DIRECTORY__
        printfn "Source File: %s" __SOURCE_FILE__
    printSourceLocation()

module trainingArea =
    open System
    
    let rec makelist l = 
        match List.length l with
        | 6 -> printfn "all done"; l
        | _ -> Console.ReadLine()::l |> makelist
    
    let l = ["hello"]
    Console.ReadLine()::l

    3::[15;23]


    makelist []

    
    
    let napiszDoKubsa =
        //prepare writer to file
        let filename = "kubs.txt"
        let path = __SOURCE_DIRECTORY__ + "/" + filename
        //let wr = new System.IO.StreamWriter(path)
        ////empty the file
        //System.IO.File.WriteAllText(path, "")

        
        //start counting trajectories
        for c in 1..count do






            //empty the file
            if c=1 then
                let wr = new System.IO.StreamWriter(path)
                System.IO.File.WriteAllText(path, "")
                wr.Close()
            else
                let wr = new System.IO.StreamWriter(path)

                let uniformRV = genRandomNumbersNominalInterval steps c
                printfn "%d" c
                let normalRV = normalizeRec uniformRV steps
            
                //build stock prices list
                let rec buildStockPricesList (currentStockPricesList:float list) (steps:int) (normalId:int) : float list =
                    let stepsMinusOne = steps - 1
                    match normalId with
                    | stepsMinusOne -> currentStockPricesList
                    | _ ->
                        let newStockPrice = currentStockPricesList.[currentStockPricesList.Length - 1] * Math.E ** (drift - ((vol**2.)/2.)*(float(years)/float(steps)) + vol * sqrt(float(years)/float(steps)) * normalRV.[normalId])
                        buildStockPricesList (currentStockPricesList @ [newStockPrice]) steps (normalId+1)
                let stockPricesList = buildStockPricesList [price] steps 0
                let finalStockPrice = stockPricesList.[stockPricesList.Length - 1]
            
                //calculate historical (realized) volatility
                let rec buildRList (rList:float list) (index:int) =
                    match index with
                    | stepsMinusOne -> rList
                    | _ ->
                        let currentR =  Math.Log((stockPricesList.[index+1])/(stockPricesList.[index]), Math.E)
                        buildRList (rList@[currentR]) (index+1)
                let rList = buildRList [] 0
                let rAvg = List.average rList
                let sumOfSquares : float = List.fold (fun acc elem -> acc  + (elem - rAvg)**2.) 0. rList
                let historicalVolatilitySquared = float(steps)/(float(years)*(float(steps)-1.)) * sumOfSquares

                //prepare final result being tuple: (finalStockPrice, realizedVolatility)
                let result = [stockPricesList.[stockPricesList.Length - 1]; historicalVolatilitySquared]

                //write trajectory to file
                System.IO.File.AppendAllText(path, string(stockPricesList.[stockPricesList.Length - 1]))
                System.IO.File.AppendAllText(path, string(historicalVolatilitySquared))
                printfn "%A" result        
                wr.Close()





