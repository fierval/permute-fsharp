let arr = [|1; 2; 3; 4|]

let findFirstLessThan (v : 'a array when 'a: comparison) pos =
    let rec findFirstSmallerPos cur =
        if cur < 0 then None
        elif v.[cur] < v.[pos] then Some cur
        else
            findFirstSmallerPos (cur - 1)
    
    findFirstSmallerPos pos

let swapPositions (v : 'a array) pos1 pos2 =
    let res = Array.copy v
    res.[pos1] <- v.[pos2]
    res.[pos2] <- v.[pos1]
    res
        
let findStartingPos v =
    let rec findStartingPosRec cur acc =
        let maxPos = snd acc
        if cur < 0 || cur < maxPos then
            if maxPos < 0 then None else Some acc
        else
            let pos = findFirstLessThan v cur
            match pos with
            | Some pos -> findStartingPosRec (cur - 1) (if maxPos < pos then (cur, pos) else acc)
            | None -> findStartingPosRec (cur - 1) acc
    findStartingPosRec (v.Length - 1)  (-1, -1)

let sortRemainder (v : 'a array when 'a: comparison) pos =
    if v.Length - 1 = pos then v
    else
        [|
            yield! v.[0..pos - 1]
            yield! Array.sort v.[pos..v.Length - 1];
        |]

let permute (v : 'a array when 'a: comparison) =
    Seq.unfold 
        (fun prev -> 
            match findStartingPos prev with
            | None -> None
            | Some (cur, pos) -> Some(prev, sortRemainder (swapPositions prev cur pos) (pos + 1))) v
    
 
let sq = permute arr 
sq |> Seq.iter (fun e -> printfn "%A" e)

    