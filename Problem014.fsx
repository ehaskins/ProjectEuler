module Problem14 = 
    let (|Even|Odd|) input = if input % 2 = 0 then Even else Odd
    
    let rec collatzLength x n = 
        match n with
        | 1 -> 1
        | _ ->
            collatzLength (x + 1) (
                match n with
                | Even ->  n / 2
                | Odd -> n * 3 + 1)
        
    let test = collatzLength 0 1000000
    
    let answer = [1 .. 1000000] |> Seq.maxBy (collatzLength 0)