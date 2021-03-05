// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open System
open System.Drawing
open System.Windows.Forms
open System.Numerics

let mandelf (c:Complex) (z:Complex) = z*z+c

let rec repeatN n f =
    match n with
     |0 -> fun x->x
     |_ -> f >> repeatN (n-1) f

let ismandel c = Complex.Abs(repeatN 20 (mandelf c) (Complex.Zero))<1.0

let rec forl a b f = 
  if a>=b then f(b)
  else 
   f(a)
   forl (a+1) b f

let scale (x:float,y:float) (u,v) n = float(n-u)/float(v-u)*(y-x)+x

let outtext = 
    forl 1 60 (fun i ->
     forl 1 60 (fun j ->
       let lscale = scale (-1.2,1.2) (1,60) 
       let t = Complex ((lscale j), (lscale i)) 
       Console.Write(if ismandel t then "*" else " ")
     )
     Console.WriteLine("")
    )

let form =
   let image = new Bitmap(400, 400)
   let lscale = scale (-1.0,1.0) (0,400)
   forl 0 (image.Height-1) (fun i ->
     forl 0 (image.Width-1) (fun j ->
       let t = Complex ((lscale i), (lscale j)) 
       image.SetPixel(i,j,if ismandel t then Color.Black else Color.White)
   )) 
   let temp = new Form()
   temp.Paint.Add(fun e -> e.Graphics.DrawImage(image, 0, 0))
   temp



[<EntryPoint>]
let main argv =
    outtext
    //do Application.Run(form)
    0 // return an integer exit code

