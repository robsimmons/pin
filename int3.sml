structure TinyIntImpl = 
struct

datatype int 
 = N4 | N3 | N2 | N1 
 | Zero 
 | P1 | P2 | P3 

end

structure TinyInt : INTEGER where type int = TinyIntImpl.int = 
struct

datatype int = datatype TinyIntImpl.int

fun toLarge i: LargeInt.int = 
   case i of 
      N4 => (~4: LargeInt.int) 
    | N3 => (~3: LargeInt.int) 
    | N2 => (~2: LargeInt.int) 
    | N1 => (~1: LargeInt.int) 
    | Zero => (0: LargeInt.int) 
    | P1 => (1: LargeInt.int) 
    | P2 => (2: LargeInt.int) 
    | P3 => (3: LargeInt.int) 

fun toLarge2 (i, j) = (toLarge i, toLarge j)

fun fromLarge (i: LargeInt.int) = 
   case i of 
      ~4 => N4
    | ~3 => N3
    | ~2 => N2
    | ~1 => N1
    | 0 => Zero
    | 1 => P1
    | 2 => P2
    | 3 => P3
    | _ => raise Overflow

val toInt = Int.fromLarge o toLarge
val fromInt = fromLarge o Int.toLarge
val precision = SOME 3
val minInt = SOME N4
val maxInt = SOME P3

val (op +) = fromLarge o LargeInt.+ o toLarge2
val (op -) = fromLarge o LargeInt.- o toLarge2
val (op * ) = fromLarge o LargeInt.* o toLarge2
val (op div) = fromLarge o LargeInt.div o toLarge2
val (op mod) = fromLarge o LargeInt.mod o toLarge2
val (op quot) = fromLarge o LargeInt.quot o toLarge2
val (op rem) = fromLarge o LargeInt.rem o toLarge2

val compare = LargeInt.compare o toLarge2
val (op <) = LargeInt.< o toLarge2
val (op <=) = LargeInt.<= o toLarge2
val (op >) = LargeInt.> o toLarge2
val (op >=) = LargeInt.>= o toLarge2

val ~ = fromLarge o LargeInt.~ o toLarge
val abs = fromLarge o LargeInt.abs o toLarge
val min = fromLarge o LargeInt.min o toLarge2
val max = fromLarge o LargeInt.max o toLarge2
val sign = LargeInt.sign o toLarge
val sameSign = LargeInt.sameSign o toLarge2

fun fmt radix i = LargeInt.fmt radix (toLarge i)
val toString = LargeInt.toString o toLarge
fun scan radix getc strm = raise Fail "Unimplemented"
fun fromString s = Option.map fromLarge (LargeInt.fromString s)

end
   
