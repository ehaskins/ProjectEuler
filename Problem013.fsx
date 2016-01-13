#load "Problem013.Resources.fs"
module Problem13 =
    let answer = 
        Problem013.Resources.numberStrings.Split '\n' 
        |> Seq.map (fun num -> bigint.Parse num) 
        |> Seq.fold (fun a b -> a + b) bigint.Zero
        |> fun bi -> bi.ToString().Substring(0, 10)