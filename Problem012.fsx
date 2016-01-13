#load "Utils.fs"

module Problem12 =
    let triangleNumbers = 
        Utils.infinite 1L |> Seq.scan (fun acc x -> acc + x) 0L |> Seq.skip 1

    let rec combinations l = 
        seq {
            match l with
            | [] ->
                yield []
            | values ->
                let first = values |> List.find (fun x -> true)
                let lc = values |> List.skip 1 |> combinations

                yield! lc |> Seq.map (fun c -> first :: c)
                yield! lc
        }
            
    let product s =
        s |> Seq.fold (fun a b -> a * b) 1L

    let factors x = 
        Utils.factorize x |> Seq.toList |> combinations |> Seq.map (fun l -> l |> product) |> Seq.distinct
    
    let result = triangleNumbers |> Seq.skip 1 |> Seq.find (fun x -> factors x |> Seq.length > 500)