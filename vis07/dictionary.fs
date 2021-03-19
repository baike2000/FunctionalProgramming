module dictionary

open System.IO
let ReadLines fn =
  seq { use inp = File.OpenText fn in
        while not(inp.EndOfStream) do
            yield (inp.ReadLine())
      };;

let table = ReadLines "C:/DiskD/users/flash/Files/University/functional_programming/2020/1.csv" 
            |> Seq.map (fun s -> s.Split([|','|]));;
let nstuds = table 
             |> Seq.fold
                 (fun x l -> if l.[0]="Y" then x+1 else x) 0;;

let FreqDict S =
   Seq.fold (
     fun (ht:Map<_,int>) v -> 
      if Map.containsKey v ht then Map.add v ((Map.find v ht) + 1) ht
      else Map.add v 1 ht)
      (Map.empty) S;;

let read = ReadLines "C:/DiskD/users/flash/Files/University/functional_programming/2020/read.txt" |> Seq.collect (fun s -> s.Split([|',';' ';':';'!';'.'|])) 
                                  |> FreqDict 
                                  |> Map.toList |> List.sortBy(fun (k1,v1) -> (k1, -v1))
                                  |> List.filter(fun (k,v) -> k.Length>3) 
                                  |> Seq.take 10;;

read






