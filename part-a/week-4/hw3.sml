exception NoAnswer

datatype pattern = Wildcard
  | Variable of string
  | UnitP
  | ConstP of int
  | TupleP of pattern list
  | ConstructorP of string * pattern

datatype valu = Const of int
  | Unit
  | Tuple of valu list
  | Constructor of string * valu

fun g f1 f2 p =
  let 
    val r = g f1 f2 
  in
    case p of
      Wildcard          => f1 ()
    | Variable x        => f2 x
    | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
    | ConstructorP(_,p) => r p
    | _                 => 0
  end

(**** you can put all your code here ****)

fun only_capitals words =
  List.filter (fn word => Char.isUpper(String.sub(word, 0))) words

fun longest_string1 words =
  List.foldl (fn (item, acc) => if String.size acc < String.size item then item else acc) "" words

fun longest_string2 words =
  List.foldl (fn (item, acc) => if String.size acc <= String.size item then item else acc) "" words

fun longest_string_helper func words = 
  List.foldl func "" words

val longest_string3 =
  longest_string_helper (fn (item, acc) => if String.size acc < String.size item then item else acc)

val longest_string4 =
  longest_string_helper (fn (item, acc) => if String.size acc <= String.size item then item else acc)

val longest_capitalized = longest_string3 o only_capitals

val rev_string = String.implode o List.rev o String.explode



