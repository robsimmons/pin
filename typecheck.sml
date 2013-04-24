structure Typecheck =
struct

open Lang

datatype succ = 
   SPos of ty
 | SNeg of ty
 | Inv of ty

fun check_value ctx v pos =
  (case (v, pos) of 
      (Var x, pos) => 
    | (Thunk n, U neg) => check_term ctx [] n (Inv neg) 
    | (Inl v, Or (pos1, pos2)) => check_value ctx v pos1
    | (Inr v, Or (pos1, pos2)) => check_value ctx v pos2
    | (PUnit, PTop) => ()
    | (PPair (v1, v2), PAnd (pos1, pos2)) =>
       ( check_value ctx v1 pos2  
       ; check_value ctx v2 pos2)
    | _ => raise TypeError)

and check_term ctx omega term u =
  (case (omega, term, u) of 
      ([], Ret v, SPos pos) => 
         check_value ctx v pos
    | ([], Apply (x, sp), SPos ty) => 
         check_spine ctx (lookup_neg x sp) sp (SPos ty)
    | ([], Apply (x, sp), SNeg ty) => 
         check_spine ctx (lookup_neg x sp) sp (SNeg ty)
    | (pos :: omega, PBind (x, sp), _) => 
         check_term ((x, Susp pos) :: ctx) omega sp u
    | (U neg :: omega, NBind (x, sp), _) =>
         check_term ((x, N neg) :: ctx) omega sp u
    | (Bot, Abort, _) => ()
    | (Or (pos1, pos2) :: omega, Case (n1, n2), _) => 
       ( check_term ctx (pos1 :: omega) n1 u
       ; check_term ctx (pos2 :: omega) n2 u)
    | (PTop :: omega, Ignore n, _) => 
         check_term ctx omega n u
    | (PAnd (pos1, pos2) :: omega, Split n, _) =>
         check_term ctx (pos1 :: pos2 :: omega) n u
    | ([], NStable n, Inv neg) => 
         check_term ctx [] n (SNeg neg)
    | ([], PStable n, Inv (F pos)) =>
         check_term ctx [] n (SPos pos)
    | ([], Lam n, Inv (Imp (pos, neg))) =>
         check_term ctx [pos] n (Inv neg)
    | ([], NUnit, Inv (NTop)) => ()
    | ([], NPair (n1, n2), Inv (NAnd (neg1, neg2))) =>
       ( check_term ctx [] n1 (Inv neg1)
       ; check_term ctx [] n2 (Inv neg2))
    | _ => raise TypeError)

and check_spine ctx neg term u = 
  (case (neg, term, u) of 
      (neg, Nil, SNeg neg) => ()
    | (PM, ) => 

end
