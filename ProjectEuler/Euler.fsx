open System

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
        
module Problem6 = 
    let sumSquareDiff values = 
        pown (values |> Seq.sum) 2 - (values |> Seq.sumBy (fun x -> pown x 2))


    let test = sumSquareDiff [1..10]
    let result = sumSquareDiff [1..100]

module Problem7 =
    let  result = Problem3.primes |> Seq.skip 10000 |> Seq.take 1 |> Seq.exactlyOne

module Problem8 =
    let number = 
        "73167176531330624919225119674426574742355349194934
 96983520312774506326239578318016984801869478851843
 85861560789112949495459501737958331952853208805511
 12540698747158523863050715693290963295227443043557
 66896648950445244523161731856403098711121722383113
 62229893423380308135336276614282806444486645238749
 30358907296290491560440772390713810515859307960866
 70172427121883998797908792274921901699720888093776
 65727333001053367881220235421809751254540594752243
 52584907711670556013604839586446706324415722155397
 53697817977846174064955149290862569321978468622482
 83972241375657056057490261407972968652414535100474
 82166370484403199890008895243450658541227588666881
 16427171479924442928230863465674813919123162824586
 17866458359124566529476545682848912883142607690042
 24219022671055626321111109370544217506941658960408
 07198403850962455444362981230987879927244284909188
 84580156166097919133875499200524063689912560717606
 05886116467109405077541002256983155200055935729725
 71636269561882670428252483600823257530420752963450"

    let digits = number.ToCharArray() |> Seq.filter (fun c -> Char.IsNumber c) |> Seq.map (fun c -> Int32.Parse (string c))

    let maxAjacentProduct windowSize = digits |> Seq.windowed windowSize |> Seq.map (fun window -> (window, window |> Array.fold (fun a b -> a * (int64 b) ) 1L)) |> Seq.maxBy (fun (set, product) -> product)

    let test = maxAjacentProduct 13

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