#load "Utils.fs"

module Problem7 =
    let  result = Utils.primes |> Seq.skip 10000 |> Seq.take 1 |> Seq.exactlyOne