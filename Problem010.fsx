#load "Utils.fs"

module Problem10 =    
    let sumPrimes primeMax = 
        Utils.primes
        |> Seq.takeWhile (fun x -> x < primeMax)
        |> Seq.sum

    let result = sumPrimes 2000000L