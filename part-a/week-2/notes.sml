(* variables *)
val x = 56
val y = 69
val z = (x + y)
val a = if x < 50 then 1 else 0
val b = abs ~5

(* functions *)
fun pow(x: int, y: int) =
    if y = 0
    then 1
    else x * pow(x, y-1)

fun square(x: int) =
    pow(x, 2)

fun cube(x: int) =
    pow(x, 3)

val s = square 2
val c = cube 3

(* pairs & tuples *)
val pair_a = (1, 2)
val tuple_a = (1, 2, 3)
val tuple_b = (1, (false, 3), 2, (0, 0, 0))

fun swap (x: int * int) =
    ((#2 x), (#1 x))

fun sum_two_pairs (x: int * int, y: int * int) =
    (#1 x) + (#2 x) + (#1 y) + (#2 y)

fun sort_pair (x: int * int) =
    if (#1 x) < (#2 x)
    then x
    else swap x

fun div_mod (x: int, y: int) =
    (x div y, x mod y)


(* lists *)
val list_0 = []
val list_a = [1, 2, 3, 4]
val list_b = 1::[2, 3]
val list_c = 1::2::3::4::[]

val head_a = hd list_a
val tail_a = tl list_a
val three = hd (tl (tl list_a))

val is_empty_0 = null list_0 (* true *)
val is_empty_a = null list_a (* false *)

fun sum_list (xs: int list) =
    if null xs
    then 0
    else hd xs + sum_list(tl xs)

fun product_list (xs: int list) =
    if null xs
    then 1
    else hd xs * product_list(tl xs)

fun countdown_list (x: int) =
    if x = 0
    then []
    else x :: countdown_list(x-1)

fun countup_list (x: int) =
    let
        fun count_up (from: int) =
            if from = x
            then x::[]
            else from::count_up(from + 1)
    in
        count_up(1)
    end


fun append_list (xs: int list, ys: int list) =
    if null xs
    then ys
    else hd xs :: append_list(tl xs, ys)

fun sum_pair_list (xs: (int * int) list) =
    if null xs
    then 0
    else #1 (hd xs) + #2 (hd xs) + sum_pair_list(tl xs)

fun firsts (xs: (int * int) list) =
    if null xs
    then []
    else #1 (hd xs) :: firsts(tl xs)


(* local variables & options  *)

fun find_max (xs: int list) =
    if null xs
    then NONE
    else if null (tl xs)
    then SOME (hd xs)
    else
        let
            val check_tail = find_max(tl xs)
        in
            if isSome check_tail andalso valOf check_tail > hd xs
            then check_tail
            else SOME (hd xs)
        end

fun find_min (xs: int list) =
    if null xs
    then NONE
    else
        let 
            fun min (xs: int list) =
                if null (tl xs)
                then hd xs
                else
                    let
                        val check_tail = min(tl xs)
                    in
                        if hd xs < check_tail
                        then hd xs
                        else check_tail
                    end
        in
            SOME (min xs)
        end


(* boolean & comparasion operations *)

val test_and = 5 > 4 andalso 3 < 2
val test_or = 5 > 4 orelse 3 < 2
val test_not = not true
val test_equal = 5 = 4
val test_not_equal = 5 <> 4