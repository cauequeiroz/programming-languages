(*
  Concepts
  ---

  First-class Functions = functions used as values
  High order functions = functions that uses functions as arguments/returns
*)

(* high order functions *)
fun n_times (func, count, item) =
  if count = 0
  then item
  else func (n_times(func, count - 1, item))

fun double x = x * 2
fun double_n_times (n, count) = n_times(double, count, n)


(* anonymous functions *)
fun tiple_n_times (n, count) =
  n_times((fn x => x * 3), count, n)


fun map (func, list) =
  case list of
    [] => []
  | x::xs => (func x)::map(func,xs)

fun filter (cond, list) =
  case list of
    [] => []
  | x::xs => if cond x
             then x::filter(cond,xs)
             else filter(cond,xs)

fun fold (func, acc, list) =
  case list of
    [] => acc
  | x::xs => fold(func, func(acc, x), xs)

val sum_all = fold(fn (acc, item) => acc + item, 0, [1, 2, 3, 4, 5])

(* closures *)
fun all_string_smaller (s, s_list) =
  let
    val s_size = String.size s
  in
    fold((fn (acc, item) => acc andalso String.size item < s_size), true, s_list)
  end

val all_string_smaller_test1 = all_string_smaller("caue", ["a", "b", "c"])
val all_string_smaller_test2 = all_string_smaller("caue", ["a", "ballon", "c"])


(* compose *)

fun compose (f, g) = fn x => (f (g x))
fun compose2 (f, g) = f o g

val two_double1 = compose(double, double) 2
val two_double2 = compose2(double, double) 2

infix !>
fun x !> f = f x

val two_double3 = 2 !> double !> double

(*  currying *)

fun sum_all_currying x = fn y => fn z => x + y + z
val sum_all_currying_test = ((sum_all_currying 1) 2) 3

fun sum_all_currying2 x y z = x + y + z
val sum_all_currying2_test = sum_all_currying2 1 2 3

fun map_with_currying func list = 
  case list of
    [] => []
  | x::xs => func x::(map_with_currying func xs)

val map_with_currying_test = map_with_currying (fn x => x * 2) [1, 2, 3]

val doubleEveryItem = map_with_currying double
val doubledItems = doubleEveryItem [1, 2, 3, 4, 5]


