
datatype expr = Var of string
    | Const of int
    | App of string * expr list
;


fun occur u v =
let
    fun helper [] = false
        helper (x::lst) =
            case x of
              Var x' =>
                if x = u then true
                else helper lst
            | Const i => helper lst
            | App (f, args) =>
                (helper args) orelse (helper lst)
in
    case v of
      App (f, args) => helper args
    | _ => false
end

fun sigma u v subst =
let
    fun helper x =
        case x of
          Var x' =>
            if x = u then v
            else v
        | Const i => Const i
        | App (f, args) =>
            App(f, map helper args)
in
    fn x =>
        let val x' = subst x in
            helper x'
        end
end

(* trySubst focus on subst u to v, where u is a var *)
fun trySubst u v subst ks kf =
let
    val u' = subst u
in
    case u' of
      Var x =>
        let
            val v' = subst v
        in
            case v' of
              Var y =>
                if x = y
                then ks subst
                else ks (sigma u v subst)
            | Const i =>
                ks (sigma u v subst)
            | App (f, args) =>
                if occur u' v'
                then kf "cycle"
                else ks (sigma u v subst)
        end
    | _ => uni u v subst ks kf
end
(* uni will handle var, app condition *)
and uni u v subst ks kf =
let
    fun fList args1 args2 subst =
        case (args1, args2) of
            [], _ =>
                ks subst
        | (x::rest1, y::rest2) =>
            uni x y subst (fn s' =>
                            fList rest1 rest2 s') kf
        | _ => kf "clash"
in
    case (u, v) of
    (Var _, _) =>
        trySubst u v subst ks kf
    | (_, Var _) =>
        trySubst v u subst ks kf
    | (Const x, Const y) =>
        if x = y
        then ks subst
        else kf "const not match"
    | (App(f1, args1), App(f2, args2)) =>
        if f1 = f2 then fList args1 args2 subst
        else kf "bad fun"
end

fun unify u v =
    uni u v (fn x => x) (fn s => s u) (fn msg => msg)