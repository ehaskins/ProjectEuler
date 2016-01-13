#load "Utils.fs"

module Problem5 =
    let factorizations values = 
        values
        |> List.map (
            fun i ->
                (i, Utils.factorize (int64 i)
                    |> Seq.groupBy(fun f -> f)
                    |> Seq.map (fun (factor, list) -> (factor, list |> Seq.length))
                )
            )

    let lcm values =
        seq {
            for value in values do
                yield! Utils.factorize (int64 value) 
                    |> Seq.groupBy(fun f -> f)
                    |> Seq.map (fun (factor, list) -> (int factor, list |> Seq.length))
        }
            |> Seq.groupBy (fun (factor, count) -> factor)
            |> Seq.map (fun (factor, list) -> list |> Seq.maxBy (fun (factor, count) -> count))
            |> Seq.fold (fun acc (factor, power) -> acc * (pown factor power)) 1

    let test = lcm [2..20] 