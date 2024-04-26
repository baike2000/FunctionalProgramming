// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
open System

printfn "%A" ((fun f -> f 0.0) int)
let g = (fun x y -> x + y) 3
printfn "%A" (g 4)
(fun (x,y) -> x + y) (3,3)




[<EntryPoint>]
let main argv =
    let n = int(Console.ReadLine())
    let r = 
      match n with 
      |12|1|2 -> "Winter"
      |3|4|5 -> "Spring"
      |_ when n >= 6 && n <=8 -> "Summer"
      |_ when n >= 9 && n <=11 -> "Autumn"
      |_ -> "Wrong number"
    printfn "%s" r  

    0 // return an integer exit code