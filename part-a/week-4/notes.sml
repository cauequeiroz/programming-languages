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
