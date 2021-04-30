// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp
// See the 'F# Tutorial' project for more help.

let rec parse x =
  match x with 
   "letrec"::id::"="::T1 -> 
     let (tr1,rem) = parse T1 in
       match rem with
        "in"::T2 -> 
          let (tr2,T3) = parse T2 in
           (Let(id,tr1,tr2),T3)
        | _ -> err
  | "fun"::id::"->"::T -> 
         let (t,r) = parse T in (Lam(id,t),r)
  | "("::T -> 
    let (t1,r1) = parse T in
      match r1 with
        ")"::T1 -> (t1,T1)
      | _ -> err
  | "<"::T -> (PFunc("<"),T)
  | "*"::T -> (PFunc("*"),T)
  | "."::T ->    
    let (t1,r1) = parse T in
    let (t2,r2) = parse r1 in
       (App(t1,t2),r2)
  | id::T -> (Const(id),T)
...;;

[<EntryPoint>]
let main argv =
    let message = from "F#" // Call the function
    printfn "Hello world %s" message
    0 // return an integer exit code
