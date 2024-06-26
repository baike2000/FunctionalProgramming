﻿type id = string
type expr = 
   Var of id
 | Lam of id*expr
 | App of expr*expr
 | Int of int
 | Cond of expr*expr*expr
 | Let of id*expr*expr
 | LetRec of id*expr*expr
 | PFunc of id
 | Op of id*int*expr list
 | Closure of expr*env
 | RClosure of expr*env*id
 | Err of id
and
 env = Map<id,expr>;;   
;;

type prog = expr;;

(* Интерпретатор лямбда-выражений *)

let arity = function
| "sin" -> 1
| otherwise -> 2


let funof = function
  "+" -> (function [Int(a);Int(b)] -> Int(a+b))
| "-" -> (function [Int(a);Int(b)] -> Int(a-b))
| "*" -> (function [Int(a);Int(b)] -> Int(a*b))
| "/" -> (function [Int(a);Int(b)] -> Int(a/b))
| "=" -> (function [Int(a);Int(b)] -> if a=b then Int(1) else Int(0))
| ">" -> (function [Int(a);Int(b)] -> if a>b then Int(1) else Int(0))
| "<" -> (function [Int(a);Int(b)] -> if a<b then Int(1) else Int(0))
| "<=" -> (function [Int(a);Int(b)] -> if a<=b then Int(1) else Int(0))

let rec eval exp env =
  printfn "eval %A" env
  match exp with
    App(e1,e2) -> apply (eval e1 env) (eval e2 env)
  | Int(n) -> Int(n)
  | Var(x) -> Map.find x env
  | PFunc(f) -> Op(f,arity f,[])
  | Op(id,n,el) -> Op(id,n,el)
  | Cond(e0,e1,e2) -> 
     if Int(1)=eval e0 env then eval e1 env else eval e2 env
  | Let(id,e1,e2) -> 
    let r = eval e1 env in
      eval e2 (Map.add id r env)
  | LetRec(id,e1,e2) ->
      eval e2 (Map.add id (RClosure(e1,env,id)) env)
  | Lam(id,ex) -> Closure(exp,env)
  | Closure(exp,env) -> exp
  |_ -> exp

and apply e1 e2 =
  printfn "app %A %A" e1 e2
  match e1 with
     Closure(Lam(v,e),env) -> eval e (Map.add v e2 env)
   | RClosure(Lam(v,e),env,id) -> eval e (Map.add v e2 (Map.add id e1 env))
   | Op(id,n,args) ->
      if n=1 then (funof id)(args@[e2])
      else Op(id,n-1,args@[e2])

let rec parse x =
  match x with 
   "letrec"::id::"="::T1 -> 
     let (tr1,rem) = parse T1 in
       match rem with
        "in"::T2 -> 
          let (tr2,T3) = parse T2 in
           (LetRec(id,tr1,tr2),T3)
        | _ -> (Err(id), T1)
  | "let"::id::"="::T1 ->
     let (tr1, rem) = parse T1 in 
     match rem with
        "in"::T2 -> 
            let (tr2,T3) = parse T2 in
            (Let(id,tr1,tr2),T3)
        | _ -> (Err(id), T1)
  | "fun"::id::"->"::T -> 
         let (t,r) = parse T in (Lam(id,t),r)
  | "("::T -> 
    let (t1,r1) = parse T in
      match r1 with
        ")"::T1 -> (t1,T1)
      | _ -> (Err(")"), T)
  | "<"::T -> 
    (PFunc("<"),T)
  | "*"::T -> 
    (PFunc("*"),T)
  | "."::T ->    
    let (t1,r1) = parse T in
    let (t2,r2) = parse r1 in
       (App(t1,t2),r2)
  | "if"::T-> 
    let (t1,r1) = parse T
    let (t2,r2) = 
        match r1 with
        | "then"::rr1 -> parse rr1
        |_ -> Err("then"), r1
    let (t3,r3) = 
        match r2 with
        | "else"::rr2 -> parse rr2
        |_ -> Err("Else"), r2
    (Cond(t1,t2,t3),r3)
  | id::T -> (Var(id),T)

let prog (txt:string list) = 
  txt |> 
  Seq.collect(fun x -> x.Split([|' '|])) |> 
  Seq.filter(fun s -> s<>"") |> 
  Seq.toList


let E exp = eval exp Map.empty;;

//let n = Let("id",Lam("x",Var("x")),Let("sq",Lam("z", App(App(PFunc("*"),Var("z")),Var("z"))),App(Var("sq"),App(Var("id"),Int(5)))))
//let d = E n
(*
letrec fact =
   fun x ->
       if <.x.1 
       then 1 
       else *.x.(fact.(-.x.1)) 
in fact.5

let sq = fun z -> *.z.z in sq.5
*)
(*let p = ["letrec fact = ";
         "   fun x -> ";
         "       if <.x.1 ";
         "       then 1 ";
         "       else *.x.(fact.(-.x.1)) ";
         "in ";"  fact.5"]*)

let p = ["let sq = fun z -> * . z . z in sq . 5"]

[<EntryPoint>]
let main argv = 
    let tokens = prog p
    printfn "%A" tokens
    let (t,_) = parse tokens
    printfn "%A" t
    printfn "%A" (E t)
    System.Console.ReadKey() |> ignore
    0 // return an integer exit code
