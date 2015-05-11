
module Euler =
    module Problem1 =
        let problem1(max) =  
            [1..max-1]
            |> List.filter(fun x -> x % 3 = 0 || x % 5 = 0)
            |> List.sum

        let p1Solution = problem1 1000

    module problem2 =
        let rec fib = Seq.unfold (fun (a,b) -> Some(a+b, (b, a+b))) (0, 1)
    
        let fibList = fib |> Seq.takeWhile (fun x -> x < 100) |> Seq.toList

        let problem2(max) =
            fib
            |> Seq.takeWhile (fun x -> x <= max)
            |> Seq.filter (fun x -> x % 2 = 0)
            |> Seq.sum

        let p2Solution = problem2 4000000
