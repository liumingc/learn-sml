let addC tS tT cset =
  if tS = tT then cset else (tS, tT)::cset

let printC cset =
  print_endline "--- beg constraint set ---";
  List.iter (fun (tyX, tyY) -> printty tyX; print_string " = "; printty tyY; print_newline ()) cset;
  print_endline "--- end constraint set ---"

let rec findC tS cset =
  match cset with
    [] -> None
  | (tS1,tT1)::rest ->
      if tS = tS1 then Some tT1
      else findC tS rest

let tCnt = ref 0
let genNewT () =
  let cur = !tCnt in
  tCnt := cur + 1;
  TyId ("?X" ^ (string_of_int cur))

(* typeofIntern : ctx term -> TyS x ConstraintSet *)
let rec typeofIntern ctx t =
  match t with
    TmVar(fi,i,_) -> (getTypeFromContext fi ctx i, [])
  | TmAbs(fi,x,tyT1,t2) ->
      let ctx' = addbinding ctx x (VarBind(tyT1)) in
      let tyT2, c1 = typeofIntern ctx' t2 in
      let tyX = TyArr (tyT2, genNewT ()) in
      (TyArr(tyT1, tyT2), (addC tyT1 tyX c1))
  | TmApp(fi,t1,t2) ->
      let tyT1, c1 = typeofIntern ctx t1 in
      let tyT2, c2 = typeofIntern ctx t2 in
      let tyX = genNewT () in
        (tyX, addC tyT1 (TyArr (tyT2, tyX)) (c1@c2))
  | TmTrue(fi) ->
      (TyBool, [])
  | TmFalse(fi) ->
      (TyBool, [])
  | TmIf(fi,t1,t2,t3) ->
      let tyT1, c1 = typeofIntern ctx t1 in
      let tyT2, c2 = typeofIntern ctx t2 in
      let tyT3, c3 = typeofIntern ctx t3 in
      let c1' = addC tyT1 TyBool c1 in
      let c2' = addC tyT2 tyT3 c2 in
        (tyT2, c1'@c2'@c3)
  | TmZero(fi) ->
      (TyNat, [])
  | TmSucc(fi,t1) ->
      let tyT1, c1 = typeofIntern ctx t1 in
      (TyNat, addC tyT1 TyNat c1)
  | TmPred(fi,t1) ->
      let tyT1, c1 = typeofIntern ctx t1 in
      (TyNat, addC tyT1 TyNat c1)
  | TmIsZero(fi,t1) ->
      let tyT1, c1 = typeofIntern ctx t1 in
      (TyBool, addC tyT1 TyNat c1)

let rec occurs tyS tyT =
  if tyS = tyT then true
  else
    match tyT with
      TyArr (ty1, ty2) ->
        occurs tyS ty1 || occurs tyS ty2
    | _ -> false

(* ConstraintSet -> subst -> subst *)
let rec unify c subst =
  match c with
    [] -> subst
  | (TyId s as tyS, tyT)::rest ->
      if tyS = tyT then subst
      else
        let sigma x =
          if x = tyS then tyT
          else if occurs tyS tyT then
            (printty tyS; print_string " and "; printty tyT; err " cycle")
          else subst x
        in
          fun x -> applySubst sigma (unify rest subst x)
  | (tyS, (TyId t as tyT))::rest ->
      let sigma x =
        if x = tyT then tyS
        else if occurs tyT tyS then
          (printty tyS; print_string " and "; printty tyT; err " cycle")
        else subst x
      in
        fun x -> applySubst sigma (unify rest subst x)
  | (TyArr (tyT11, tyT12), TyArr (tyT21, tyT22)):: rest ->
      unify ((tyT11, tyT21)::(tyT12, tyT22)::rest) subst
  | (TyBool, TyBool)::rest ->
      unify rest subst
  | (TyNat, TyNat)::rest ->
      unify rest subst
  | (tyS, tyT)::_ ->
      (
        printty tyS;
        print_string " and ";
        printty tyT;
        err " cannot unify"
      )
and applySubst subst ty =
  match ty with
    TyBool -> TyBool
  | TyNat -> TyNat
  | TyId s as tyS ->
      subst tyS
  | TyArr (tyS, tyT) ->
      TyArr (subst tyS, subst tyT)

let rec typeof ctx t =
  let ty, c = typeofIntern ctx t in
    printC c;
    printty ty;
    print_string " => \n";
    let subst = unify c (fun x -> x) in
    let ans = applySubst subst ty in
    printty ans;
    print_newline ();
    ans
