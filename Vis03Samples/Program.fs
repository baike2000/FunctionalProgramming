// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp
// See the 'F# Tutorial' project for more help.

(*type 'a List =
    | ([])
    | (::) of 'a * 'a List
*)

let rec length l =
    match l with
    |[]    -> 0
    |_::t -> 1+length(t)

List.length [1;2;3;4;5]

let rec lengthF = function 
    []     -> 0
    | _::t -> 1+lengthF(t)

let rec sumF = function
    | [x] -> x
    | x::t -> x+sumF t


let rec sum L =
    match L with
    | [x] -> x
    | x::t -> x+sum t

let rec mem x = function
    [] -> false
    | z::t -> z=x || mem x t

List.iter2(fun x y -> printfn "%A.%s" x y) [1;2;3] ["One";"Two";"Three"]

let rec append M L = 
  match M with
    [] -> L
  | h::t -> h::(append t L)

let rec remove x = function
    [] -> []
    | h::t when h=x -> remove x t 
    | h::t -> h::remove x t

let rec iter f = function
      [] -> ()
    | h::t -> (f h); iter f t

type person = string*int;;
let plist = [("Vasya",123);("Petya",234)];;
let find_name no = List.tryFind (fun (name,num) -> num=no) plist
let findh = 
    match (find_name 123) with
         None -> "No person found"
        |Some((name,num)) -> "The person is "+name

let rec map f = function
    [] -> []
    | h::t -> (f h)::map f t;;

let rec filter p = function
    [] -> []
    | x::t when p(x) -> x::filter p t
    | x::t -> filter p t

let sumFold = List.fold (fun ac x -> ac+x) 0
let prodFold = List.fold (fun ac x -> ac*x) 1
let sumByInt f = List.fold (fun ac x ->ac*(f x)) 0
let sumRed = List.reduce (fun u v -> u+v)

let exists p = List.fold (fun a x -> a || x) false
let forall p = List.fold (fun a x -> a && x) true

let rec insertions x = function
    [] -> [[x]]
    | h::t -> (x::h::t)::(List.map (fun z -> h::z) (insertions x t))

let rec permute L = 
    match L with
          [] -> [[]]
        | h::t -> List.collect (fun z -> insertions h z) (permute t)

let perm = permute[1..4]
let fact n = List.length (permute [1..n])
fact 5
let rec primes l = 
   match l with
   | [] -> []
   | h::t -> h::primes (List.filter (fun x -> x%h>0) t)

let primeslist = primes [2..100]

let rec fold_list f = function
    | [] -> []
    | h::t -> h::fold_list f (f h t)

let primesF = fold_list (fun h l -> List.filter (fun x -> x%h>0) l)

type image = char list list;;

let mklist (s:string) = [ for c in s.ToCharArray() -> c ]
let sample = [
                mklist "...........";
                mklist "...##......";
                mklist "..#..#.....";
                mklist ".....##....";
                mklist ".....##....";
                mklist "....#.##...";
                mklist "...##..##..";
                mklist "..##....##.";
                mklist ".##.....##.";
                mklist ".##.....##.";
                mklist "..........."]

let flipH = List.rev
let flipV : image->image = List.map List.rev
let stack : image->image->image = (@)
let glue  : image->image->image = List.map2 (@)
let inverse : image->image = List.map (List.map (fun c -> if c='.' then '#' else '.'))

inverse sample

let rec revN = function
    [] -> []
    | h::t -> (revN t)@[h]

let rev L =
   let rec rv s = function
         [] -> s
       | h::t -> rv (h::s) t 
   rv [] L



[<EntryPoint>]
let main argv =
    let l = System.Console.ReadLine().ToCharArray() 
            |> Array.map(fun x ->  int(x) - int('0')) 
            |> List.ofArray 
            |> List.rev
    
    let (s,a) = l |> List.fold(fun (s,a) x -> 
                             (s + (if s = "" then "" else "+") + x.ToString(), a+x ))
                             ("",0)

    printf "%s\r\n%A" s a
    0
