module montecarlo

let generate openf compute closef = 
    seq { let r = openf() 
          try 
            let x = ref None
            while (x := compute r; (!x).IsSome) do
                yield (!x).Value
          finally
             closef r }

let rand max n = generate (fun () -> new System.Random(n)) 
                          (fun r -> Some(r.NextDouble()*max)) 
                          (fun _ -> ())

let MonteCarlo hit max iters =
  let hits = (float)(
      Seq.zip (rand max 1) (rand max 3) |>
      Seq.take iters |>
      Seq.filter hit |>
      Seq.length) 
  4.0*max*max*hits/((float)iters)

let area radius =
 MonteCarlo
  (fun (x,y) -> x*x+y*y<=radius*radius)
  radius 10000000

let Pi = (area 1000.0)/1000000.0
