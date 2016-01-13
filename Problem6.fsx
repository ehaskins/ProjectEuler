module Problem6
    let sumSquareDiff values = 
        pown (values |> Seq.sum) 2 - (values |> Seq.sumBy (fun x -> pown x 2))

    let test = sumSquareDiff [1..10]
    let result = sumSquareDiff [1..100]