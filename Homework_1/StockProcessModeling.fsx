open System

module Generator =

    //Draw independent standard normal random variables using Box-Muller transformation
    let generateSNV (steps: int)(random: Random) =
        let listOfNumbers = Array.create steps 0.
        let mutable i = 0
        while (i<steps) do
            let U1 = random.NextDouble()
            let U2 = random.NextDouble()
            let Z1 = Math.Sqrt((-2.) * Math.Log(U1, Math.E)) * Math.Sin(2. * Math.PI * U2)
            listOfNumbers.[i]<-Z1
            if i<(steps-1) then
                let Z2 = Math.Sqrt((-2.) * Math.Log(U1, Math.E)) * Math.Cos(2. * Math.PI * U2)
                i <- i+1
                listOfNumbers.[i]<-Z2
            i <- i+1
        listOfNumbers        

    //Defining csv type, which will be used to save generated numbers into .csv file
    type csvType = { Index:int; SNV:double }
                    override this.ToString() = 
                        sprintf "%d;%f\n" this.Index this.SNV

    //Function used to save generated numbers into .csv file to check, that they are truly normally distributed
    let generateSaveToFile (steps: int)(seed: int) =
        let writer = new System.IO.StreamWriter("generatedSNV.csv")
        let random = new Random(seed)
        let indexedArray = Array.create steps {Index = 0; SNV = 0.}
        let indexer (list: array<float>) =
            for i in 1..steps do
                let record = {Index = i; SNV = list.[(i-1)]}
                indexedArray.[(i-1)]<-record
        generateSNV steps random |> indexer

        indexedArray |> Array.map(string) |> String.concat("") |> writer.Write
        writer.Close()

//Test generator module
Generator.generateSaveToFile (1000)(5)


module Simulation =
    //Type used to represent one row in the output file: two columns - one for Final Stock Price, one for Realized Volatility
    type csvType = { FinalStockPrice:double; RealizedVolatility:double }
                    override this.ToString() = 
                        sprintf "%f;%f\n" this.FinalStockPrice this.RealizedVolatility

    let simulate (count: int)(steps: int)(price: double)(drift: double)(vol: double)(years: double)(seed: int) =
        let writer = new System.IO.StreamWriter("output.txt")
        let random = new Random(seed)
        let timeInterval = years / ((float)steps)
        let outputArray = Array.create count {FinalStockPrice = 0.; RealizedVolatility = 0. }

        for i in 0..(count-1) do
            let returns = Array.create steps 0.
            let mutable actPrice = price
            let mutable retSum = 0.
            let standardNormalVariables = Generator.generateSNV steps random

            for j in 1..steps do
                let power = (drift - ((vol * vol)/2.))*timeInterval + vol*Math.Sqrt(timeInterval)*standardNormalVariables.[(j-1)]
                let newPrice = actPrice * Math.Pow(Math.E, power)
                let ret = Math.Log((newPrice/actPrice), Math.E)
                returns.[(j-1)]<-ret
                retSum <- retSum + ret
                actPrice <- newPrice

            let retMean = retSum / (float) steps
            let mutable sum = 0.

            for j in 0..(steps-1) do
                sum <- sum + Math.Pow(returns.[j] - retMean, 2)

            let variance = ((float)steps / (years * (float)(steps-1))) * sum
            let HT = Math.Sqrt(variance)

            let outputRecord = {FinalStockPrice = actPrice; RealizedVolatility = HT}
            outputArray.[i]<-outputRecord

        outputArray |> Array.map(string) |> String.concat("") |> writer.Write
        writer.Close()

Simulation.simulate 1500 300 100 0.05 0.2 1. 5
//test simulation with vol=0
Simulation.simulate 1500 300 100 0.05 0. 1. 5


