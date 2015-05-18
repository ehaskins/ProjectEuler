module Problem7
    let  result = Problem3.primes |> Seq.skip 10000 |> Seq.take 1 |> Seq.exactlyOne