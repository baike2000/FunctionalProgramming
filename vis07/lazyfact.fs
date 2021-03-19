module lazyfact

let lazyFactorial n = Lazy.Create (fun () ->
    let rec factorial n =
        printfn "Apply %d" n
        match n with
        | 0 | 1 -> 1
        | n -> n * factorial (n - 1)
    factorial n)
let f = lazyFactorial 8
f.Force()

let rec lazy_fact n = 
 match n with
   1 -> lazy 1
 | n -> lazy (n* lazy_fact(n-1)));;

let rec lazy_fact1 n = 
 lazy(
 let _ = printfn "Apply %d" n in
 match n with
   1 -> 1
 | n -> n*lazy_fact(n-1).Force());;



let rec fib (n:int64) =
  if n<2L then 1L else fib (n-1L) + fib(n-2L);;

open System.Collections.Generic
let mfib =
   let d = new Dictionary<int64,int64>()
   let rec fib n =
     if d.ContainsKey(n) then d.[n]
     else
       let res = if n<2L then 1L else fib (n-1L) + fib(n-2L)
       d.Add(n,res)
       res
   fun n -> fib n;;

open System
 
let duration f = 
    let timer = new System.Diagnostics.Stopwatch()
    timer.Start()
    let returnValue = f()
    printfn "Elapsed Time: %i" timer.ElapsedMilliseconds
    returnValue
duration (fun() -> fib 40L);;
duration (fun() -> mfib 40L);;

open System.Collections.Generic
let memoize (f: 'a -> 'b) =
   let t = new Dictionary<'a,'b>()
   fun n ->
     if t.ContainsKey(n) then t.[n]
     else let res = f n
          t.Add(n,res)
          res;;

let rec fibFast =
       memoize (
         fun n -> if n < 2 then 1 
                  else fibFast(n-1) + fibFast(n-2));;

fibFast 20


