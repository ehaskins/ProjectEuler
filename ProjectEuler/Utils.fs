module Utils
    let infinite init = Seq.unfold (fun x -> Some(x, x+1L)) init

    let isPrime x =
        let rec check i = double i > sqrt (double x) || (x % i <> 0L && check (i + 1L))

        if x > 1L then
            check 2L
        else
            false            

    let rec primes = infinite 1L |> Seq.filter (fun x -> isPrime x)