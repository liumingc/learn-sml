(* use mosml
* This example is taken from 6.1 of <<Types and Programming Languages>>.
* *)
load "Int";

datatype lterm = Lvar of string
  | Llam of string * lterm list
  | Lapp of lterm list
  ;


(* de bruijin index *)
datatype brterm = Bind of int (* indice *)
  | Blam of brterm list
  | Bapp of brterm list
  ;

fun toBruijin env (x : lterm) =
(
  case x of
     Lvar x =>
      (let val idx = lookup env x
       in Bind idx
       end)
     | Llam (x, lst) =>
        let
          val env' = extend env x 
        in
          Blam (map (fn elt => toBruijin env' elt) lst)
        end
     | Lapp lst =>
         Bapp (map (fn elt => toBruijin env elt) lst)
)

and lookup env x =
let
  fun helper env i =
    case env of
         [] => 9999 (* not found *)
       | y::env' =>
           if x = y
           then i
           else helper env' (i+1)
in
  helper env 0
end

and extend env x =
  x :: env
  ;

fun printLam (x : lterm) =
  case x of
       Lvar x =>
        print x
     | Llam (x, lst) =>
         (print "/\\ ";
          print x;
          print " . ";
          app (fn elt => (printLam elt; print " ")) lst)
    | Lapp lst =>
        ( print "(";
        app (fn elt => (printLam elt; print " ")) lst;
        print ")" )
        ;

fun printBru (x : brterm) =
  case x of
       Bind i =>
        print (Int.toString i)
     | Blam lst =>
         (print "/\\ ";
          print ". ";
          app (fn elt => (printBru elt; print " ")) lst)
     | Bapp lst =>
         (print "(";
          app (fn elt => (printBru elt; print " ")) lst;
          print ")")
          ;

val inputList = [
                  Llam ("s", [Llam ("z", [Lvar "z"])]),
                  Llam ("s", [Llam ("z", [Lapp [Lvar "s", Lapp [Lvar "s", Lvar
                  "z"]]])])
                ]

val _ =
  app (fn x =>
        (print "Lambda : \n";
         printLam x;
         print "\nBruijin : \n";
         printBru (toBruijin [] x);
         print "\n"
         ))
      inputList


