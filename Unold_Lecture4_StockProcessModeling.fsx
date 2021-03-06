module GemetricBrownianMotion

open System

//Script implementing simulation of single stock pracies as values of Gemoetric Brownian Motion with given drift and volatility

module helpers =
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
    let writeRVToFile (fileName:string) (result:float list) =
        let path = __SOURCE_DIRECTORY__ + "/" + fileName
        let wr = new System.IO.StreamWriter(path)
        result |> List.map(string) |> String.concat("\n") |> wr.Write
        wr.Close()

    //let normalRV = normalizeRec (genRandomNumbersNominalInterval 10000 40000) 10000 |> writeRVToFile "hubert40000.csv"

    //function that writes result to a file
    let writeResultToFile (fileName:string) (result:float list list) =
        let path = __SOURCE_DIRECTORY__ + "/" + fileName
        let wr = new System.IO.StreamWriter(path)
        for p in result do
            //printfn "%A" p
            p |> List.map(string) |> String.concat(",") |> wr.Write
            "\n" |> wr.Write
        wr.Close()

module GBM =
    open helpers
    open System

    //writeResultToFile [[15.; 14.; 78.];[15.; 14.; 78.]] "siemka.txt"
    let simulateGBM (count:int) (steps:int) (price:float) (drift:float) (vol:float) (years:int) (seed:int) =
        //start counting t(trajectories)
        let rec buildResult currentResult t =
            if t = count+1 then currentResult
            else
                let normalRV = normalizeRec (genRandomNumbersNominalInterval steps t) steps
        
                //build stock prices list
                let rec buildStockPricesList (currentStockPricesList:float list) (steps:int) (normalId:int) : float list =
                    if normalId = steps-1 then currentStockPricesList
                    else
                        let firstExpTerm =  (drift - (vol**2.)/2.) * (float(years)/float(steps))
                        let secondExpTerm =  vol * sqrt(float(years)/float(steps)) * normalRV.[normalId]
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
        result

    let count = 1000
    let steps = 250 //must be EVEN!
    let price = 4.20
    let drift = 0.12
    let vol = 0.
    let years = 1
    let seed = 5
    
    simulateGBM count steps price drift vol years seed |> writeResultToFile "output.csv"

module BlackScholesModel =
    open helpers
    open System
    //Params for Geometric Brownian Motion used for simulating stock prices
    type GBMParams = 
        {
            //count:int
            steps:int
            price:float
            drift:float
            vol:float
            years:float
            seed:int
        }
    //Params for Black-Scholes Model for pricing an option
    type BSParams = 
        {
            k: float    //strike
            m: float    //maturity
        }

    //simulate and predict option price
    let simulateBlackScholesPutOptionPriceAndDelta_old (gbm:GBMParams) (bs:BSParams) =
        let normalRV = normalizeRec (genRandomNumbersNominalInterval gbm.steps gbm.seed) gbm.steps
        //build stock prices list
        let rec buildStockPricesList (currentStockPricesList:float list) (steps:int) (normalId:int) : float list =
            if normalId = steps-1 then currentStockPricesList
            else
                let firstExpTerm =  (gbm.drift - (gbm.vol**2.)/2.) * (float(gbm.years)/float(steps))
                let secondExpTerm =  gbm.vol * sqrt(float(gbm.years)/float(steps)) * normalRV.[normalId]
                let newStockPrice = currentStockPricesList.[normalId] * Math.E ** (firstExpTerm + secondExpTerm)
                buildStockPricesList (currentStockPricesList@[newStockPrice]) steps (normalId+1)
        let stockPricesList = buildStockPricesList [gbm.price] gbm.steps 0
        let finalStockPrice = stockPricesList.[stockPricesList.Length - 1]
        stockPricesList

    let cfd (mean,stdev,point) = 
        point * 0.87

    //simulate and predict option price
    let simulateBlackScholesPutOptionPriceAndDelta (gbm:GBMParams) (bs:BSParams) =
        let normalRV = normalizeRec (genRandomNumbersNominalInterval gbm.steps gbm.seed) gbm.steps
        //build stock prices list
        let rec buildStockPricesList (currentStockPricesList:float list) (steps:int) (normalId:int) : float list =
            if normalId = steps-1 then currentStockPricesList
            else
                let firstExpTerm =  (gbm.drift - (gbm.vol**2.)/2.) * (float(gbm.years)/float(steps))
                let secondExpTerm =  gbm.vol * sqrt(float(gbm.years)/float(steps)) * normalRV.[normalId]
                let newStockPrice = currentStockPricesList.[normalId] * Math.E ** (firstExpTerm + secondExpTerm)
                buildStockPricesList (currentStockPricesList@[newStockPrice]) steps (normalId+1)
        let stockPricesList = buildStockPricesList [gbm.price] gbm.steps 0
        let finalStockPrice = stockPricesList.[stockPricesList.Length - 1]

        let d1 = (Math.Log(gbm.price/bs.k, Math.E) + (gbm.drift + 0.5*(gbm.vol**2.))*bs.m) / (gbm.vol*sqrt(bs.m))
        let BScall = 
            let d2 = d1 - gbm.vol*sqrt(bs.m)
            let BScallPrice = gbm.price * cfd(0, 1, d1) - (bs.k/Math.E**(gbm.drift*bs.m) * cfd(0, 1, d1))
            BScallPrice
        let BScallDelta =
            cfd(0,1,d1)
        let BSputDelta = BScallDelta - 1.
        let BSput =
            BScall + bs.k/(Math.E**(gbm.drift*bs.m)) - gbm.price
        //[BScall; BScallDelta; BSput; BSputDelta]
        //BScall
        //BSput
        BScallDelta
        //BSputDelta



    let g2 = {
        years=1.
        steps=200
        price=4.2
        drift=0.14
        vol=0.1
        seed=5}
    let b2 = {
        k=8.
        m=1.}

    simulateBlackScholesPutOptionPriceAndDelta g2 b2




    let g = {
        years=1.
        steps=200
        price=1.
        drift=0.14
        vol=0.2
        seed=5}
    let b = {
        k=8.
        m=0.001}
    let createList =
        let rec buildList currentResult g b step =
            if step = 100 then currentResult
            else
                let r = simulateBlackScholesPutOptionPriceAndDelta g b
                let newTapl = [g.price; r]
                let resNew = currentResult@[newTapl]
                let gNew = { years=g.years; steps=g.steps; price=g.price+1.; drift=g.drift; vol=g.vol; seed=g.seed}
                let stepNew = step + 1
                buildList resNew gNew b stepNew
        buildList [] g b 1


    let a = createList

    a |> writeResultToFile "output0001.csv"

    

module GBM_old = 
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





