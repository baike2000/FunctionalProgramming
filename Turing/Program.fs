// Learn more about F# at http://fsharp.org

open System

type based = char;;
type state = int*int;;
type ribbon = int -> based;;
type op = L | R | U | H;;
type instr = state * based * based * state * op;;
type program = instr list;;


let empty = fun x -> ' ';;
let ins c n rib = fun x ->
  if n=x then c else rib x;;

let mkrib (s:string) =
  let rec f n = function
    [] -> empty
  | h::t -> ins h n (f (n+1) t) in
  s.ToCharArray() |> List.ofArray |> (f 0);;

type MT = program * state * ribbon;;


let proc n = function R -> n+1 | L -> n-1 | U -> n | H -> -1 ;;

let step (P,(sym,st),rib) =
  let s = rib sym in
  let (_,_,nchr,nst,cmd) = List.find(fun (q,c,_,_,_) -> (q=st)&&(c=s)) P in
  let nsym = proc sym cmd in
  (P,(nsym,nst),ins nchr sym rib);;

let terminal MT =
  match MT with
    (_,(n,_),_) when n<0 -> true
  | _ -> false;;

let print (P,(sym,st),rib) =
  let aux n i (c:char) = if i=n then "["+c.ToString()+"]" else c.ToString() in
  printfn "q=%2d, p=%2d %s" st sym
    ([ for i in 0..10 -> aux sym i (rib i) ]
      |> List.fold(fun ac c -> ac+c) "");;

let rec run MT =
  if terminal MT then MT
  else
     let n = step MT in
     let _ = print n in
     run (n);;


let mtadd =
    (
    [ (0,'0','0',0,R);
       (0,'1','1',0,R);
       (0,' ',' ',1,R);
       (1,'0','0',1,R);
       (1,'1','1',1,R);
       (1,' ',' ',2,L);
       (2,'1','0',3,L);
       (2,'0','1',2,L);
       (2,' ',' ',5,R);
       (3,'1','1',3,L);
       (3,'0','0',3,L);
       (3,' ',' ',4,L);
       (4,'1','0',4,L);
       (4,'0','1',0,R);
       (4,' ',' ',0,R);
       (5,'1',' ',5,R);
       (5,' ',' ',5,H)],
       (0,0),mkrib "001110 101");;

[<EntryPoint>]
let main argv = 
    run mtadd |> ignore
    System.Console.ReadLine() |> ignore
    0 // return an integer exit code



