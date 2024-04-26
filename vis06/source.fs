module Source

let mutable x = 3;;
let adder y = x + y;;
adder 1;;
x <- 4;;
adder 1;;

let def_adder n = fun x -> x + n
let add5 = def_adder 5
printfn "%A" (add5 5)

let rec Acc m n =
 if m = 0 then 
    n + 1 
 else 
    if n = 0 then 
        Acc (m-1) n
    else Acc (m-1) (Acc m (n - 1))

let A3 = Acc 3
printfn "%A" (A3 10)

type cell = { mutable content : int }

let new_counter n = 
    let x = {content = n} 
    fun () -> 
        x.content <- x.content + 1
        x.content

let cnt = new_counter 0
cnt()

let new_generator fgen init =
    let x = ref init 
    fun () -> x := fgen !x
              !x
let fibgen = new_generator (fun (u,v) -> (u+v,u)) (1,1)

fibgen ()

let mapg f gen = 
    let g = gen
    fun () -> f(g())
    
let rec repeat cond gen = 
    let x = gen ()
    if cond x then x else repeat cond gen

let fgen = mapg (fun (u,v) -> u) fibgen
repeat (fun x -> (x % 32)) fgen
