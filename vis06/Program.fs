open System

type reg = string * int


type args = Regs of string * string | RegNum of string * int 

type command =   Mov of args
               | Xchg of string * string
               | Label of string * int
               | Loop of string * int
               | Nop
               | Exit
type commandticks = command * int 
             
type program = command array
type registers = reg list

type state =
    {
         regs : registers;
         ticks : int;
         runline : int;
         exit : bool; 
         tickslist : commandticks list;
    }

let findticks (l:commandticks list)  (c:command) = 
    let (_, r) = l |> List.find(fun x -> 
                            match (x) with
                             |cmd, t when c = cmd -> true
                             |_ -> false
                           )
    r


let getreg (st:state) regname = 
    st.regs |> List.find(fun (x, y) -> x = regname)

let setreg (st:state) a b = 
    let regs = st.regs |> List.map(fun (reg, vl) -> if (reg = a) then (reg,b) else (reg,vl))    
    {regs = regs; ticks = st.ticks;  runline = st.runline; exit = st.exit; tickslist = st.tickslist}

let mov args (st:state) =
    let t = findticks (st.tickslist) (Mov(Regs("","")))
    match args with
    |Regs (a,b) -> let (_,vb) = getreg st b
                   let cs = setreg st a vb
                   {regs = cs.regs; ticks = cs.ticks +  t;  runline = cs.runline + 1; exit = cs.exit; tickslist = cs.tickslist}
    |RegNum (a,b) -> let cs = setreg st a b 
                     {regs = cs.regs; ticks = cs.ticks +  t;  runline = cs.runline + 1; exit = cs.exit; tickslist = cs.tickslist}

let xchg a b (st:state) = 
    let t = findticks (st.tickslist) (Xchg ("",""))
    let (_, vb) = getreg st b
    let (_, vq) = getreg st a
    let cs = setreg (setreg st a vb) b vq
    {regs = cs.regs; ticks = cs.ticks +  t;  runline = cs.runline + 1; exit = cs.exit; tickslist = cs.tickslist}

let loop line cur (st:state)=
    let t = findticks (st.tickslist) (Loop ("",-1))
    let cs = 
        match getreg st "C" with
        |(_,c) when c > 0 ->let cs = setreg st "C" (c-1) 
                            {regs = cs.regs; ticks = cs.ticks; runline = line; exit = cs.exit; tickslist = cs.tickslist}
        |_ -> {regs = st.regs; ticks = st.ticks; runline = cur; exit = st.exit; tickslist = st.tickslist}
    {regs = cs.regs; ticks = cs.ticks +  t;  runline = cs.runline + 1; exit = cs.exit; tickslist = cs.tickslist}

let nop st = 
    let t = findticks (st.tickslist) (Nop)
    let cs = st
    {regs = cs.regs; ticks = cs.ticks +  t;  runline = cs.runline + 1; exit = cs.exit; tickslist = cs.tickslist}

let exit st = 
    let t = findticks (st.tickslist) (Exit)
    {regs = st.regs; ticks = st.ticks + t;  runline = st.runline; exit = true; tickslist = st.tickslist}

let label st =
    {regs = st.regs; ticks = st.ticks;  runline = st.runline+1; exit = st.exit ; tickslist = st.tickslist}

let rec run (prog:program) (st : state) =
    match st.exit with
        | true -> st
        | _ -> match prog.[st.runline] with
                | Mov args -> run prog (mov args st)
                | Xchg (a,b) -> run prog (xchg a b st)
                | Label (s,l) -> run prog (label st)
                | Loop (_, line) -> run prog (loop line st.runline st) 
                | Nop -> run prog (nop st)
                | Exit ->run prog (exit st)

let findlabel (prog:program) label =
    prog |> Array.findIndexBack(fun x -> 
                                    match x with
                                     | Label (n,l) -> n = label
                                     | _ -> false
                               )

let parse i (cmd:string)  =
    match cmd with
     |"EXIT" -> Exit
     |_ when cmd.IndexOf(":") >= 0 -> Label (cmd.Substring(cmd.IndexOf(":")+1), i)
     |_ when cmd.IndexOf("MOV") >= 0 -> 
                                        let c = cmd.Substring(cmd.IndexOf("MOV") + 3).Split(',') 
                                                |> Array.map(fun x -> x.Trim()) |> Array.filter(fun x -> not (String.IsNullOrEmpty(x)))
                                        if (["A";"B";"C";"D"] |> List.exists(fun x -> x = c.[1])) 
                                        then Mov (Regs (c.[0],c.[1])) 
                                        else Mov (RegNum (c.[0], int(c.[1])))
     |_ when cmd.IndexOf("XCHG") >= 0 -> 
                                        let c = cmd.Substring(cmd.IndexOf("XCHG") + 4).Split(',') 
                                        Xchg (c.[0].Trim(), c.[1].Trim())
     |_ when cmd.IndexOf("LOOP") >= 0 -> 
                                        let c = cmd.Substring(cmd.IndexOf("LOOP") + 4)
                                        Loop (c.Trim(), -1)
     |_ when cmd.IndexOf("NOP") >= 0 -> Nop
     |_ when cmd.IndexOf("EXIT") >= 0 -> Exit
     |_ -> Label ("",-1)

let read _ = Console.ReadLine()
let isValid = function null -> false | _ -> true

[<EntryPoint>]
let main argv = 
   let cmdticks = [Mov (Regs("","")); Xchg ("",""); Loop ("",-1); Nop;  Exit] 
                    |> List.map(fun x -> x, int(Console.ReadLine()))
   let initprog = Seq.initInfinite read 
                  |> Seq.takeWhile isValid 
                  |> Seq.mapi(parse)
                  |> Seq.toArray 
              
   let prog = initprog |> Array.map(fun x -> 
                                        match x with 
                                        |Loop (n,_) -> Loop (n, findlabel initprog n)
                                        |_ -> x
                                    )
   let initstate = 
        { 
            regs = [("A", 0);("B", 0);("C", 0);("D", 0)];
            ticks = 0;
            runline = 0;
            exit = false;
            tickslist = cmdticks;
        }

   let res = run prog initstate
   printfn "%A" res.ticks
   0 // return an integer exit code

