open System
open System.Drawing

(* from int to float range *)
let scale (a:float,b:float) (i,j) x = 
    float(x-i)/float(j-i)*(b-a)+a

(* from float range to int *)
let uscale (i,j) (a:float,b:float) (f:float) = 
    int((f-a)/(b-a)*float(j-i))+i
let xrange = (0,50);;
let yrange = (0,15);;

let plot xdim ydim f =
  for i=fst yrange to snd yrange do
    for j=fst xrange to snd xrange do
      let x = scale xdim xrange j
      let y = uscale yrange ydim (f x)
      Console.Write(if y=i then "*" else ".")
    Console.WriteLine("")

let rec fora a b i f =
  if a>=b then i
  else f (fora (a+1) b i f) a

let plot1 xdim f = 
    let ydim = fora (fst xrange) (snd yrange) (100.0,-100.0) 
                    (fun (xmin,xmax) i ->
                         let x = scale xdim xrange i
                         (min xmin (f x),max xmax (f x))
                    ) 
    for i=fst yrange to snd yrange do
      for j=fst xrange to snd xrange do
          let x = scale xdim xrange j
          let y = uscale yrange ydim (f x)
          // printf "i=%d, j=%d, x=%f, y=%f\n" i j x y
          Console.Write(if y=i then "*" else ".")
      Console.WriteLine("")

type out_device = int*(int*int -> unit);;

let plotgen xdim f dev =
  let (n,dfun) = dev
  let ydim = fora 0 n (100.0,-100.0) 
                  (fun (xmin,xmax) i ->
                       let x = scale xdim xrange i
                       (min xmin (f x),max xmax (f x)))
  for i=0 to n-1 do
    for j=0 to n-1 do
      let x = scale xdim (0,n-1) j
      let y = uscale (0,n-1) ydim (f x)
      if y=i then dfun (j,y)

let implot xdim f =
    let img = new Bitmap(400,400)
    plotgen xdim f (400, 
      fun (x,y) -> img.SetPixel(x,y,Color.White))
    img.Save(@"c:\image.bmp")

let cplot xdim f = 
    plotgen xdim f (40, 
    
            fun (x,y) ->  for i in 1..x do
                               for j in 1..y do
                                    if (i=x && j=y) then System.Console.Write("*") else System.Console.Write(" ")
                               System.Console.WriteLine();
                     )




[<EntryPoint>]
let main argv = 
    plot (-4.0,4.0) (-1.0,1.0) log
    ////plot1 (-4.0,4.0) sin
    //implot (-4.0,4.0) sin
    //cplot (-4.0,4.0) sin

    Console.ReadKey() |> ignore;
    0 // return an integer exit code
