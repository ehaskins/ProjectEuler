module Problem4
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