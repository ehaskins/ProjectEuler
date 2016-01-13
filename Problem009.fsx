module Problem9 =
    let triplets factorMax = 
        seq{
            for a in [1.0..factorMax] do
                for b in [a..factorMax] do
                    yield (a, b, sqrt (a ** 2.0 + b ** 2.0))
        }

    let specialTriplets sum = 
        triplets sum
        |> Seq.filter (fun (a, b, c) -> a < b && b < c)
        |> Seq.filter (fun (a, b, c) -> a ** 2.0 + b ** 2.0 = c ** 2.0)
        |> Seq.find (fun (a, b, c) -> a + b + c = sum)

    let result = specialTriplets 1000.0 |> fun (a,b,c) -> a * b * c