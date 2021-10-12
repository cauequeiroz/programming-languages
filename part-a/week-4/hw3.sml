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

fun first_answer func elements =
  case elements of
    [] => raise NoAnswer
  | x::xs => case func x of
              SOME v => v
            | NONE => first_answer func xs

fun all_answers func elements =     
  let
    fun loop lst acc =
      case lst of
        [] => SOME acc
      | x::xs => case func x of
                        NONE => NONE
                      | SOME i => loop xs (acc @ i)
  in  
    case elements of
      [] => SOME []
    | x::xs => loop elements []
  end

val count_wildcards =
  g (fn () => 1) (fn x => 0)

val count_wild_and_variable_lengths =
  g (fn () => 1) (fn x => String.size x)

fun count_some_var (name, p) =
  g (fn () => 0) (fn x => if x = name then 1 else 0) p

fun check_pat p = 
  let
    fun get_variable_list p =
      case p of
        Variable x        => [x]
      | TupleP ps         => List.foldl (fn (p, acc) => (get_variable_list p) @ acc) [] ps
      | ConstructorP(_,p) => get_variable_list p
      | _                 => []

    fun only_unique lst =
      case lst of
        [] => true
      | head::tail => if (List.exists (fn x => x = head) tail)
                      then false
                      else only_unique tail
  in
    only_unique (get_variable_list p)
  end


fun match (v, p) = SOME []
fun first_match v ps = SOME []
