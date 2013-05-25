structure Types =
struct

datatype ty =
   U of ty                   (* vA- or U A- *)
 | Bot                       (* 0 *)
 | Or of ty * ty             (* A+ + B+ *)
 | PTop                      (* 1 *)
 | PAnd of ty * ty           (* A+ * B+ *)
 | F of ty                   (* ^A+ or F A+ *)
 | Imp of ty * ty            (* A+ -> B- *)
 | NTop                      (* T *)
 | NAnd of ty * ty           (* A- & B- *)

type t = ty

(*[ datasort pos = 
       U of neg
     | Bot
     | Or of pos * pos
     | PTop 
     | PAnd of pos * pos
 
    and neg = 
       F of pos
     | Imp of pos * neg
     | NTop 
     | NAnd of neg * neg ]*)

datatype ctxelem = N of ty | Susp of ty

(*[ datasort ctxelem' = N of neg | Susp of pos ]*)
(*[ sortdef ctx = (string * ctxelem') list ]*)

fun eq (x, y) = 
   case (x, y) of 
      (U x, U y) => eq (x, y)
    | (Bot, Bot) => true
    | (Or (x1, x2), Or (y1, y2)) => eq (x1, y1) andalso eq (x2, y2)
    | (PTop, PTop) => true
    | (PAnd (x1, x2), PAnd (y1, y2)) => eq (x1, y1) andalso eq (x2, y2)
    | (F x, F y) => eq (x, y)
    | (Imp (x1, x2), Imp (y1, y2)) => eq (x1, y1) andalso eq (x2, y2)
    | (NTop, NTop) => true
    | (NAnd (x1, x2), NAnd (y1, y2)) => eq (x1, y1) andalso eq (x2, y2) 
    | _ => false   

end
