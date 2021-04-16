// Learn more about F# at http://fsharp.org

open System

type id = string
type expr = 
   Var of id
 | Lam of id*expr
 | App of expr*expr
 | PFunc of (expr->expr)
 | AT
 | Int of int
 | Closure of id*expr*env
 
and env = Map<id,expr>


type stack = expr list
type control = expr list
type dump = (stack*env*control) list
type state = stack*env*control*dump

let rec eval (SECD:state) = 
  printfn "%A" SECD
  match SECD with
    (Res::S, E, C, D) when (C |> List.isEmpty) && (D |> List.isEmpty) -> Res
  | (x::S, E, C, (S1,E1,C1)::D) when (C |> List.isEmpty) -> eval (x::S1,E1,C1,D)
  | (S,E,Var(z)::C,D) -> eval ((Map.find z E)::S,E,C,D)
  | (S,E,Lam(z,b)::C,D) -> eval (Closure(z,b,E)::S,E,C,D)
  | (S,E,Int(x)::C,D) -> eval(Int(x)::S,E,C,D)
  | (S,E,PFunc(x)::C,D) -> eval(PFunc(x)::S,E,C,D)
  | (Closure(x,b,E1)::(arg::S),E,AT::C,D) -> 
      eval ([],Map.add x arg E1,[b],(S,E,C)::D)
  | (PFunc(f)::(arg::S),E,AT::C,D) -> eval (f(arg)::S,E,C,D)
  | (S,E,App(f,a)::C,D) -> eval (S,E,a::(f::(AT::C)),D)

let succ a = a 

let E = eval ([],Map.empty,[App(App(PFunc(succ),Int(1)),Int(2))],[])


[<EntryPoint>]
let main argv =
    E
    0 // return an integer exit code
