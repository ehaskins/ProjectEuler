#load "Utils.fs"
open Operators.Checked

module Problem14 = 
    let (|Even|Odd|) input = if input % 2L = 0L then Even else Odd

    let rec collatzLength = 
        let rec core = (fun pos n ->
                                match n with
                                | 1L -> pos + 1L
                                | _ ->
                                    core (pos + 1L) (
                                        match n with
                                        | Even ->  n / 2L
                                        | Odd -> n * 3L + 1L))
        fun n -> core 0L n
        
    let test = collatzLength 13L
    
    let answer = [1L..1000000L] |> List.maxBy collatzLength