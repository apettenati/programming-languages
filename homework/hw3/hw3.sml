exception NoAnswer

datatype pattern = WildcardP
                 | VariableP of string
                 | UnitP
                 | ConstantP of int
                 | ConstructorP of string * pattern
                 | TupleP of pattern list

datatype valu = Constant of int
              | Unit
              | Constructor of string * valu
              | Tuple of valu list

fun g f1 f2 p =
    let 
        val r = g f1 f2 
    in
        case p of
            WildcardP         => f1 ()
          | VariableP x       => f2 x
          | ConstructorP(_,p) => r p
          | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
          | _                 => 0
    end

fun only_lowercase (sl: string list) =
    List.filter (fn s => Char.isLower(String.sub(s, 0))) sl

fun longest_string1 (sl: string list) =
    List.foldl (fn (x,y) => 
        if String.size x > String.size y then x else y) "" sl

fun longest_string2 (sl: string list) =
    List.foldl (fn (x,y) => 
        if String.size x >= String.size y then x else y) "" sl

fun longest_string_helper f sl =
    List.foldl (fn (x,y) => 
                    if (f (String.size x, String.size y))
                    then x
                    else y
               )
               "" sl

val longest_string3 = longest_string_helper(op>)

val longest_string4 = longest_string_helper(op>=)

val longest_lowercase = longest_string1 o only_lowercase

val caps_no_X_string =
      String.implode
    o (List.filter (fn c => not (c = #"X")))
    o String.explode
    o (String.map Char.toUpper)

fun first_answer f xs =
    case foldl (fn (x, prev) => if (isSome prev) then prev else f x) NONE xs of
    NONE => raise NoAnswer
      | SOME v => v