
module Problem1 =
    let problem1(max) =  
        [1..max-1]
        |> List.filter(fun x -> x % 3 = 0 || x % 5 = 0)
        |> List.sum

    let p1Solution = problem1 1000

module Problem2 =
    let rec fib = Seq.unfold (fun (a,b) -> Some(a+b, (b, a+b))) (0, 1)
    
    let fibList = fib |> Seq.takeWhile (fun x -> x < 100) |> Seq.toList

    let problem2(max) =
        fib
        |> Seq.takeWhile (fun x -> x <= max)
        |> Seq.filter (fun x -> x % 2 = 0)
        |> Seq.sum

    let p2Solution = problem2 4000000

module Problem3 =
    let infinite init = Seq.unfold (fun x -> Some(x, x+1L)) init

    let isPrime x =
        let rec check i = double i > sqrt (double x) || (x % i <> 0L && check (i + 1L))

        if x > 1L then
            check 2L
        else
            false            

    let rec primes = infinite 1L |> Seq.filter (fun x -> isPrime x)

    let rec factorize x =
        let prime = primes |> Seq.find (fun prime -> x % prime = 0L)
        
        if x = 1L then
            seq { yield 1L }
        else
            seq {
                yield prime;
                if x / prime <> 1L then yield! (factorize (x / prime))
            }

    let factorization = factorize 600851475143L |> Seq.toList
    
module Problem4 = 
    let twoDigitNumbers = [10..99]
    let threeDigitNumbers = [100..999]

    let rec digits x = 
        seq {
            if x >= 10 then
                yield x % 10
                yield! digits (x / 10)
            else
                yield x
        }

    let isPalindromic x = 
        let d = digits x |> Seq.toList
        
        not ([0 .. d.Length / 2 - 1] 
            |> List.exists (fun i -> d.Item i <> d.Item (d.Length - (i + 1))))

    let crossProduct list = 
        seq {
            for a in list do
                for b in list do
                    yield a * b}

    let palindromes list =
        crossProduct list |> Seq.filter (fun x -> isPalindromic x)

    let twoDigitMax = palindromes twoDigitNumbers |> Seq.max
    let threeDigitMax = palindromes threeDigitNumbers |> Seq.max

module Problem5 =
    let factorizations values = 
        values
        |> List.map (
            fun i ->
                (i, Problem3.factorize (int64 i)
                    |> Seq.groupBy(fun f -> f)
                    |> Seq.map (fun (factor, list) -> (factor, list |> Seq.length))
                )
            )

    let lcm values =
        seq {
            for value in values do
                yield! Problem3.factorize (int64 value) 
                    |> Seq.groupBy(fun f -> f)
                    |> Seq.map (fun (factor, list) -> (int factor, list |> Seq.length))
        }
            |> Seq.groupBy (fun (factor, count) -> factor)
            |> Seq.map (fun (factor, list) -> list |> Seq.maxBy (fun (factor, count) -> count))
            |> Seq.fold (fun acc (factor, power) -> acc * (pown factor power)) 1

    let test = lcm [2..20] 
        

