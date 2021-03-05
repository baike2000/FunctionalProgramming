module sum_even

let sum_even L = L |> List.filter(fun x -> x % 2 = 0) |> List.sum

sum_even [1..10]

let rec fact = function 
  1 -> 1
  | x -> x*fact(x-1)

fact 10

let rec quicksort L = 
    match L with
        [] -> []
        |h::t -> quicksort (List.filter(fun x -> x <= h) L) 
                 @ [h] @ quicksort (List.filter(fun x -> x > h) L)

quicksort [10;9;8;7;6;5;4;3;2;1]
