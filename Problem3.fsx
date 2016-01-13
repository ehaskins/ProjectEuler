module Problem3
    open Utils

    let rec factorize x =
        let prime = Utils.primes |> Seq.find (fun prime -> x % prime = 0L)
        
        if x = 1L then
            seq { yield 1L }
        else
            seq {
                yield prime;
                if x / prime <> 1L then yield! (factorize (x / prime))
            }

    let factorization = factorize 600851475143L |> Seq.toList