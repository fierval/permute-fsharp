﻿#r @"C:\Program Files (x86)\FSharpPowerPack-4.0.0.0\bin\FSharp.PowerPack.dll"
open Microsoft.FSharp.Math
open System.Numerics

type Microsoft.FSharp.Collections.Map<'Key, 'Value when 'Key : comparison> with
    static member mapValues (f : 'Value -> 'U) x =
        x |> Map.toSeq |> Seq.map (fun (k, v) -> (k, f v))

let uniform (v : int seq) =
    let v = v |> Seq.toList
    v |> Seq.map (fun i -> i, 1N / BigRational.FromInt v.Length) |> Map.ofSeq

type IndEventBuilder () =
    member this.Zero () = Map.empty.Add(0, 0.)
    member this.Return (e : 'a) : Map<'a, BigRational> = Map.empty.Add(e, 1N)
    member this.Bind (m : Map<'a, BigRational>, fn : 'a -> Map<'b, BigRational>) : Map<'b, BigRational> =
        m 
        |> Map.toSeq |> Seq.map (fun (key, value) -> (fn key) |> Map.mapValues (fun x -> x * value))
        |> Seq.concat
        |> Seq.groupBy(fun (key, value) -> key) 
        |> Seq.map(fun (key, sq) -> 
                    (key, sq |> 
                            Seq.map (fun (key, value) -> value) |> Seq.sum)) 
        |> Map.ofSeq
            

let independent = IndEventBuilder()

let mp = independent {
    let! x = uniform [1..6]
    let! y = uniform [1..6]
    let! z = uniform [1..6]
    return x + y + z
}

let range = 52
let cardProb = uniform [1..range / 4]

let redDog = 
    let removeCard (v : BigRational) (i : BigInteger) = 
        if v = 0N then v
        else
            let mult = BigInteger range / v.Denominator
            BigRational.FromBigInt (v.Numerator * mult - i) / BigRational.FromBigInt  (v.Denominator * mult - BigInteger.One)

    let distWithRemoved card dist = dist |> Map.map (fun key v -> if key <> card then removeCard v BigInteger.Zero else removeCard v BigInteger.One)

    independent {
        let! card1 = cardProb
        let removedCard = distWithRemoved card1 cardProb
        let! card2 = removedCard
        let! card3 = distWithRemoved card2 removedCard 

        let minCard = min card1 card2
        let maxCard = max card1 card2

        if card1 = card2 then
            if card2 = card3 then return 10 else return 0
        elif minCard + 1 = maxCard then return 0
        elif minCard < card3 && card3 < maxCard then return 1 else return -1
    }

let simpleRedDog = 
    independent {
        let! card1 = cardProb
        let! card2 = cardProb
        let! card3 = cardProb

        let minCard = min card1 card2
        let maxCard = max card1 card2

        if card1 = card2 then
            if card2 = card3 then return 10 else return 0
        elif minCard + 1 = maxCard then return 0
        elif minCard < card3 && card3 < maxCard then return 1 else return -1
    }

let expect dist = dist |> Map.toSeq |> Seq.sumBy (fun (k, v) -> float k * BigRational.ToDouble v)
let verify (dist : Map<int, BigRational>) = dist |> Map.toSeq |> Seq.sumBy (fun (k, v) -> v) = 1N
