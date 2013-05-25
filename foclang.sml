structure Lang =
struct

datatype exp =             
   Var of int                (* z *)
 | Thunk of exp              (* thunk N *)
 | Inl of exp                (* inl V *)
 | Inr of exp                (* inr V *)
 | PUnit                     (* <> *)
 | PPair of exp * exp        (* <V1, V2> *)

 | Ret of exp                (* ret V *)
 | Apply of int * exp        (* x Sp *)
 | PBind of string * exp     (* z.N *)
 | NBind of string * exp     (* x.N *)
 | Abort                     (* abort *)
 | Case of exp * exp         (* [N1, N2] *)
 | Ignore of exp             (* <>.N *)
 | Split of exp              (* *N *)
 | NStable of exp            (* <<N>> *)
 | PStable of exp            (* {N} *)
 | Lam of exp                (* \N *)
 | NUnit                     (* <> *)
 | NPair of exp * exp        (* <N1, N2> *)
 | PCut of exp * exp         (* V (+) N,
                              * V focused on A+,
                              * N introducing A+ with stable type *)
 | NCut of exp * exp         (* M (-) Sp,
                              * M introducing A-
                              * Sp focused on A- *)

 | Nil                       (* *)
 | PM of exp                 (* matches N *)
 | App of exp * exp          (* V Sp *)
 | ProjL of exp              (* .proj1 Sp *)
 | ProjR of exp              (* .proj2 Sp *)

(*[ datasort value = 
       Var of int
     | Thunk of term
     | Inl of value
     | Inr of value
     | PUnit
     | PPair of value * value

    and term = 
       Ret of value
     | Apply of int * spine
     | PBind of string * term
     | NBind of string * term
     | Abort
     | Case of term * term
     | Ignore of term
     | Split of term
     | NStable of term
     | PStable of term
     | Lam of term
     | NUnit
     | NPair of term * term
     | PCut of term * term
     | NCut of term * term

    and spine = 
       Nil
     | PM of term
     | App of value * spine
     | ProjL of spine
     | ProjR of spine ]*)

end
