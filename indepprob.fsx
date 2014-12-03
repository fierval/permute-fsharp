let mapValues f m = m |> Map.toSeq |> Seq.map (fun (k, v) -> (k, f v))

let uniform (v : int seq) =
    let v = v |> Seq.toList
    v |> Seq.map (fun i -> i, (float 1) / (float v.Length)) |> Map.ofSeq

type IndEventBuilder () =
    member this.Zero () = Map.empty.Add(0, 0.)
    member this.Return (e : 'a) : Map<'a, float> = Map.empty.Add(e, 1.)
    member this.Bind (m : Map<'a, float>, fn : 'a -> Map<'a, float>) : Map<'a, float> =
        seq {
            for (key, value) in m |> Map.toSeq do 
                yield! mapValues (fun x -> x * value) (fn key)
            }
            |>  Seq.groupBy(fun (key, value) -> key) |> Seq.map(fun (key, sq) -> (key, sq |> Seq.map (fun (key, value) -> value) |> Seq.sum)) |> Map.ofSeq
            

let independent = IndEventBuilder()

let mp = independent {
    let! x = uniform [1..6]
    let! y = uniform [1..6]
    let! z = uniform [1..6]
    return x + y + z
}

let redDog = 
    let cardProb = uniform [1..13]
    independent {
        let! card1 = cardProb
        let! card2 = cardProb
        let! card3 = cardProb

        let firstCard = min card1 card2
        let secondCard = max card1 card2

        if card1 = card2 then
            if card1 = card2 && card2 = card3 then return 10 else return 0
        elif firstCard + 1 = secondCard then return 0
        elif firstCard < card3 && card3 < secondCard then return 1 else return -1
    }
