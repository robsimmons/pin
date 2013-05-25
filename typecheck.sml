structure Typecheck =
struct

open Types
open Lang
exception TypeError

datatype succ = 
   SPos of ty
 | SuspNeg of ty
 | Inv of ty

(*[ datasort succ' =
       SPos of pos
     | SuspNeg of neg
     | Inv of neg ]*)

(*[ val lookup_neg : ctx * int -> neg ]*)
fun lookup_neg (ctx, x) = 
   case (ctx, x) of 
      ([], x) => raise Fail "bad index"
    | ((_, N neg) :: _, 0) => neg
    | ((_, Susp _) :: _, 0) => raise TypeError
    | (_ :: ctx, n) => lookup_neg (ctx, n-1)

(*[ val lookup_pos : ctx * int -> pos ]*)
fun lookup_pos (ctx, x) = 
   case (ctx, x) of 
      ([], x) => raise Fail "bad index"
    | ((_, N _) :: _, 0) => raise TypeError
    | ((_, Susp pos) :: _, 0) => pos
    | (_ :: ctx, n) => lookup_pos (ctx, n-1)  

(*[ val check_value : ctx -> value -> pos -> unit ]*)
(*[ val check_term : ctx -> pos list -> term -> succ' -> unit ]*)
(*[ val check_spine : ctx -> neg -> spine -> succ' -> unit ]*)

fun check_value ctx v pos =
  (case (v, pos) of 
      (Var x, pos) => 
         if Types.eq (lookup_pos (ctx, x), pos) then () else raise TypeError
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
         check_spine ctx (lookup_neg (ctx, x)) sp (SPos ty)
    | ([], Apply (x, sp), SuspNeg ty) => 
         check_spine ctx (lookup_neg (ctx, x)) sp (SuspNeg ty)
    | (pos :: omega, PBind (x, sp), _) => 
         check_term ((x, Susp pos) :: ctx) omega sp u
    | (U neg :: omega, NBind (x, sp), _) =>
         check_term ((x, N neg) :: ctx) omega sp u
    | (Bot :: omega, Abort, _) => ()
    | (Or (pos1, pos2) :: omega, Case (n1, n2), _) => 
       ( check_term ctx (pos1 :: omega) n1 u
       ; check_term ctx (pos2 :: omega) n2 u)
    | (PTop :: omega, Ignore n, _) => 
         check_term ctx omega n u
    | (PAnd (pos1, pos2) :: omega, Split n, _) =>
         check_term ctx (pos1 :: pos2 :: omega) n u
    | ([], NStable n, Inv neg) => 
         check_term ctx [] n (SuspNeg neg)
    | ([], PStable n, Inv (F pos)) =>
         check_term ctx [] n (SPos pos)
    | ([], Lam n, Inv (Imp (pos, neg))) =>
         check_term ctx [pos] n (Inv neg)
    | ([], NUnit, Inv (NTop)) => ()
    | ([], NPair (n1, n2), Inv (NAnd (neg1, neg2))) =>
       ( check_term ctx [] n1 (Inv neg1)
       ; check_term ctx [] n2 (Inv neg2))
    | _ => raise TypeError)

and check_spine ctx neg sp u = 
  (case (neg, sp, u) of 
      (neg, Nil, SuspNeg neg') => 
         if Types.eq (neg, neg') then () else raise TypeError
    | (F pos, PM n, u) => check_term ctx [pos] n u
    | (Imp (pos, neg), App (v, sp), _) => 
       ( check_value ctx v pos 
       ; check_spine ctx neg sp u)
    | (NAnd (neg1, neg2), ProjL sp, _) => check_spine ctx neg1 sp u
    | (NAnd (neg1, neg2), ProjR sp, _) => check_spine ctx neg2 sp u
    | _ => raise TypeError) 

end
