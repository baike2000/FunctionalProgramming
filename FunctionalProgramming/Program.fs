// Learn more about F# at http://fsharp.org

open System
type Doc = SSN of int | Passport of string 
type Person = string*int*Doc
let vasya = ("Vasya",14,SSN 1234)
let leha = ("Leha",18,Passport "AN786544")
let _,_,doc = leha
let getdoc x = 
    match x with
    |_,_,SSN i -> printfn "%d" i
    |_,_,Passport s -> printfn "%s" s

getdoc vasya
getdoc leha

let rec fora a b i f =
    if (a>=b) then i
    else f (fora (a+1) b i f) a

printfn "%A" (fora 1 10 0 (fun a i -> a + i))
[<EntryPoint>]
let main argv =
    
    let plus x y = x + y
    let plus1 = plus 1 
    plus1 2

    let s f = f 0.

    let f (x,y) = x+y

    let even x = if x % 2 = 0 then "Even" else "Odd"
    even 3
    even 4





    //Console.ReadKey |> ignore
    0 // return an integer exit code



