open TextIO

fun camlCase () =
  (* read in a line, and output a line with caml case *)
  let
    val upper = ref true
    fun cvt () =
      case input1 stdIn of
           NONE => ()
         | SOME #"\n" => ()
         | SOME c =>
             (
             if Char.isAlpha c then
               if !upper then
                 (
                 print (Char.toString (Char.toUpper c));
                 upper := false
                 )
               else
                 print (Char.toString (Char.toLower c))
             else if c = #"_" then
               upper := true
             else
               print (Char.toString c);
             cvt ()
             )
  in
    cvt ()
  end
