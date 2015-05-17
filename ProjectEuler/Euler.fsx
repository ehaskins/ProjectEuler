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

module Problem10 = 
    let sumPrimes primeMax = 
        Problem3.primes
        |> Seq.takeWhile (fun x -> x < primeMax)
        |> Seq.sum

    let result = sumPrimes 2000000L

module Problem11 =
    let grid = [|
        [|08; 02; 22; 97; 38; 15; 00; 40; 00; 75; 04; 05; 07; 78; 52; 12; 50; 77; 91; 08;|];
        [|49; 49; 99; 40; 17; 81; 18; 57; 60; 87; 17; 40; 98; 43; 69; 48; 04; 56; 62; 00;|];
        [|81; 49; 31; 73; 55; 79; 14; 29; 93; 71; 40; 67; 53; 88; 30; 03; 49; 13; 36; 65;|];
        [|52; 70; 95; 23; 04; 60; 11; 42; 69; 24; 68; 56; 01; 32; 56; 71; 37; 02; 36; 91;|];
        [|22; 31; 16; 71; 51; 67; 63; 89; 41; 92; 36; 54; 22; 40; 40; 28; 66; 33; 13; 80;|];
        [|24; 47; 32; 60; 99; 03; 45; 02; 44; 75; 33; 53; 78; 36; 84; 20; 35; 17; 12; 50;|];
        [|32; 98; 81; 28; 64; 23; 67; 10; 26; 38; 40; 67; 59; 54; 70; 66; 18; 38; 64; 70;|];
        [|67; 26; 20; 68; 02; 62; 12; 20; 95; 63; 94; 39; 63; 08; 40; 91; 66; 49; 94; 21;|];
        [|24; 55; 58; 05; 66; 73; 99; 26; 97; 17; 78; 78; 96; 83; 14; 88; 34; 89; 63; 72;|];
        [|21; 36; 23; 09; 75; 00; 76; 44; 20; 45; 35; 14; 00; 61; 33; 97; 34; 31; 33; 95;|];
        [|78; 17; 53; 28; 22; 75; 31; 67; 15; 94; 03; 80; 04; 62; 16; 14; 09; 53; 56; 92;|];
        [|16; 39; 05; 42; 96; 35; 31; 47; 55; 58; 88; 24; 00; 17; 54; 24; 36; 29; 85; 57;|];
        [|86; 56; 00; 48; 35; 71; 89; 07; 05; 44; 44; 37; 44; 60; 21; 58; 51; 54; 17; 58;|];
        [|19; 80; 81; 68; 05; 94; 47; 69; 28; 73; 92; 13; 86; 52; 17; 77; 04; 89; 55; 40;|];
        [|04; 52; 08; 83; 97; 35; 99; 16; 07; 97; 57; 32; 16; 26; 26; 79; 33; 27; 98; 66;|];
        [|88; 36; 68; 87; 57; 62; 20; 72; 03; 46; 33; 67; 46; 55; 12; 32; 63; 93; 53; 69;|];
        [|04; 42; 16; 73; 38; 25; 39; 11; 24; 94; 72; 18; 08; 46; 29; 32; 40; 62; 76; 36;|];
        [|20; 69; 36; 41; 72; 30; 23; 88; 34; 62; 99; 69; 82; 67; 59; 85; 74; 04; 36; 16;|];
        [|20; 73; 35; 29; 78; 31; 90; 01; 74; 31; 49; 71; 48; 86; 81; 16; 23; 57; 05; 54;|];
        [|01; 70; 54; 71; 83; 51; 54; 69; 16; 92; 33; 48; 61; 43; 52; 01; 89; 19; 67; 48;|];
    |]

    let product s =
        s |> Seq.fold (fun a b -> a * b) 1

    let products length lines = 
        lines 
        |> Seq.windowed length
        |> Seq.map (fun window -> window |> product)

    let horizontalProducts length grid =
        seq{
            for line in grid do
                yield! line |>  products length
        }

    let column index (grid:'a[][]) = 
        grid
        |> Array.map (fun line -> line.[index])

    let diagonal length (dirX, dirY) (baseX, baseY) =
        seq{
            for i in [0..length-1] do
                yield grid.[baseX+dirX*i].[baseY+dirY*i]
        }

        
    let diagonals length (grid:'a[][]) =
        let height = Array.length grid
        let width = Array.length grid.[0]

        seq {
            for x in 0..width-1 do
                for y in 0..height-length do
                    if x < width - length then
                        yield diagonal length (1, 1) (x, y)
                    if x > length - 1 then
                        yield diagonal length (-1, 1) (x, y)
        }

    let diagonalProducts length grid =
        diagonals length grid |> Seq.map (fun diag -> diag |> product)

    let verticalProducts length (grid:int [][]) =
        seq {
            for i in [0..(Array.length grid.[0]) - 1] do
                yield! grid |> column i |> products length
            }

    let allProducts length grid = 
        seq{
            yield! horizontalProducts length grid
            yield! verticalProducts length grid
            yield! diagonalProducts length grid

        }

    let result = allProducts 4 grid |> Seq.max

module Problem12 =
    let triangleNumbers = 
        Problem3.infinite 1L |> Seq.scan (fun acc x -> acc + x) 0L |> Seq.skip 1

    let rec combinations l = 
        seq {
            match l with
            | [] ->
                yield []
            | values ->
                let first = values |> List.find (fun x -> true)
                let lc = values |> List.skip 1 |> combinations

                yield! lc |> Seq.map (fun c -> first :: c)
                yield! lc
        }
            
    let product s =
        s |> Seq.fold (fun a b -> a * b) 1L

    let factors x = 
        Problem3.factorize x |> Seq.toList |> combinations |> Seq.map (fun l -> l |> product) |> Seq.distinct
    
    let result = triangleNumbers |> Seq.skip 1 |> Seq.find (fun x -> factors x |> Seq.length > 500)
