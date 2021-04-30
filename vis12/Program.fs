// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System

type Nondet<'T> = 'T list
let ret x = [x]
let return' x = x
let fail = []
let (>>=) mA (b:'b->Nondet<'b>) = List. map b mA |> List.concat;;

type NondetBuilder() =
  member b.Return(x) = ret x
  member b.Bind(mA,x) = mA >>= x
  member b.Zero() = fail
  member b.Combine(a,x) = a@x
  member b.Delay(x) = x()

let nondet = NondetBuilder()
    
let lev = [false;false;false;true;true;true;true]
let edi = [true;true;true;false;false;false;true]
let days = ["mon";"tue";"wed";"thu";"fri";"sat";"sun"]

let data = List.zip3 lev edi days

let rec prev last hit l = match l with
    [] -> last
  | h::t -> if hit h then last else prev h hit t

let realday state said = 
    if state then said else not(said)

let task1 = async {return 10+10}
let task2 = async {return 20+20}

let rec fib x = 
    match x with
    |_ when x = 1 -> 1
    |_ when x = 0 -> 0
    |_ -> fib(x-1) + fib(x-2)

let s = seq { for i in 1..30 do async {return fib(i)}}
let res = s |> Async.Parallel |> Async.RunSynchronously

let map' func items =
    let tasks =
        seq {
            for i in items -> async { return (func i) }
         }
    tasks |> Async.Parallel |> Async.RunSynchronously

let r1 = map' fib {1..30}

[<EntryPoint>]
let main argv =
    let res =
      nondet {
        let! (l,e,d) = data in
             let (l1,e1,d1) = prev (true,true,"sun") (fun (_,_,x) -> x=d) data in
                 if (realday l false) = l1 && (realday e false) = e1 then return (l,e,d)
      } 
    printfn "%A" res
    0 // return an integer exit code