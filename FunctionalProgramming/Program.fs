// Learn more about F# at http://fsharp.org

open System

[<EntryPoint>]
let main argv =
    let n = System.Console.ReadLine() |> int
    let v = System.Console.ReadLine().Trim().Split(' ') |> Array.map int |> List.ofArray
    let x = System.Console.ReadLine() |> int
    let r1 = v |> List.fold (fun a n -> if x = n then 1+a else a) 0
    printfn "%A" r1

    //Console.ReadKey |> ignore
    0 // return an integer exit code



