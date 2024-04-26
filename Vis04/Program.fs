// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.


type ExprTree = Op of char * ExprTree * ExprTree 
                    | Value of int

let rec compute = function
        | Value(x) -> x
        | Op(op, L, R) -> 
                    match op with
                     |'+' -> compute L + compute R 
                     |'-' -> compute L - compute R 
                     |'*' -> compute L * compute R 
                     |'/' -> compute L / compute R 

let priority = function 
     |'+' -> 1
     |'-' -> 1 
     |'*' -> 2 
     |'/' -> 2
     |_ -> 100

let lastop (s:string) = 
    let expr = s.ToCharArray() |> List.ofArray 
    let (res,_,_,_) = expr |> 
                          List.fold(fun (r,c,p,i) x -> 
                                  match x, c with
                                   |'(', _ -> (r,c+1,p,i+1)
                                   |')', _ -> (r,c-1,p,i+1)
                                   |_, 0 when priority(x) <= p -> (i,c,priority(x),i+1)
                                   |_ -> (r,c,p,i+1) 
                          ) (-1,0,50,0)
    res
     
let rec parse (expr:string) = 
    let k = lastop expr
    match expr.[0],expr.[expr.Length-1], k with
        |'(',')', -1 -> parse (expr.Substring(1,expr.Length-2))
        |_, _, -1 -> Value(int(expr))
        |_ -> Op(expr.[k],parse(expr.Substring(0,k)),parse(expr.Substring(k+1)))


[<EntryPoint>]
let main argv = 
   let res = System.Console.ReadLine().Split('=') 
                |> Array.map parse 
                //|> Array.iter (fun x -> printfn "%A" x)                
                |> Array.map compute 
   //printfn "%A" res
                |> Array.reduce(-) 
   printfn "%s" (if res = 0  then "YES" else "NO")
   0 // return an integer exit code

//(1+2*3)/2=4+5*8/2
//3=24
