module taylor

open System

let iter f a b i = List.fold f i [a..b]

let fact1 x = [1..x] |> List.fold (fun a y -> a*y) 1 

let rec iterf f a b i =
  if a>b then i
  else f (iter f (a+1) b i) a
  
let pow x n = iter (fun y i -> y*x) 1 n 1.0
let fact n = iter (fun y i -> y*(float i)) 1 n 1.0

let taylor x n = pow x n / fact n;;

let myexp x =
  iter (fun y n -> y+taylor x n) 0 15 0.0



myexp 10.0

