module samples

let fibs = Seq.unfold (fun (u,v) -> Some(u,(u+v,u))) (1,1);;

Seq.take 10 fibs;;
fibs;;

let squares = Seq.initInfinite(fun n -> n*n);;
let squares10 = Seq.init 10 (fun n -> n*n);;
squares10 |> Seq.toList

Seq.take 10 squares;;



