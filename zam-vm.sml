
datatype instr = IGRAB of int
               | RESTART
               | APPLY of int
               | RETURN of int
               | APPTERM of int * int
and value = VINT of int
               | VSTR of string
               | VCLOS of instr list * int
               ;

type mach =
  { pc: instr list,
    stack: value list,
    env: value list,
    accu: value,
    extra: int
   }
;


fun run (m: mach) =
(
  case #pc m of
       [] =>
      print "End"
     | APPLY n :: rest =>
         (
          print ("APPLY<" ^ (Int.toString n) ^ ">")
         )
     | _ =>
      print "Instr not supported"
)
;


