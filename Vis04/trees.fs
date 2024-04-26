module trees

type 'T tree = 
  Leaf of 'T
| Node of 'T*('T tree list)

let tr = Node(1,[
            Node(2,[
               Leaf(5)]);
            Node(3,[
               Leaf(6);
               Leaf(7)]);
            Leaf(4)])

let rec iter f = function
  Leaf(T) -> f(T)
| Node(T,L) -> (f(T); L |> List.iter (fun x-> iter f x))

let iterh f = 
  let rec itr n = function
      Leaf(T) -> f n T
    | Node(T,L) -> (f n T; for t in L do itr (n+1) t done) in
    itr 0

let spaces n = if n>0 then [for i in 1..n do yield " "] |> List.reduce(+) else ""

let print_tree T = iterh (fun h x -> printfn "%s%A\n" (spaces (h*3)) x) T

print_tree tr

type 't btree = Node of 't * 't btree * 't btree | Nil 
let tr1 = Node(6,
            Node(3,
               Node(1,Nil,Nil),
               Node(4,Nil,Nil)),
            Node(7,Nil,Nil))

let prefix root left right = (root(); left(); right());;
let infix root left right = (left(); root(); right());;
let postfix root left right = (left(); right(); root());;

let iterh1 trav f t = 
  let rec tr t h =
    match t with
     |Node (x,L,R) -> trav 
                        (fun () -> (f x h)) 
                        (fun () -> tr L (h+1)) 
                        (fun () -> tr R (h+1));
     |Nil -> ()
  tr t 0

let print_tree1 T = iterh1 postfix (fun x h -> printf "%s%A\n" (spaces h) x) T;;
print_tree1 tr1


let fold_infix init f t = 
    let rec tr t x =
        match t with
            Node (z,L,R) -> tr R (f z (tr L x))
            |Nil -> x
    tr t init

let tree_to_list T = fold_infix [] (fun x l -> x::l) T;;
tree_to_list tr1


open System.IO
let rec tree path ind =
   Directory.GetDirectories path |>
   Array.iter(fun dir ->
     printfn "%s%s" (spaces (ind*3)) dir;
     tree dir (ind+1)
     );;
tree @"C:\DiskD\users\flash\Files\University" 0

let rec du path =
   Directory.GetDirectories path |>
   Array.iter(fun dir ->
     let sz = Directory.GetFiles dir |>
                Array.map(fun f -> new FileInfo(f)) |>
                Array.fold (fun ac x -> ac+x.Length) 0L;
     printfn "%10d %s" sz dir;
     du dir
     );;
du @"C:\DiskD\users\flash\Files\University"






