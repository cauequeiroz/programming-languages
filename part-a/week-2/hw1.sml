(* 
    Year is PositiveNatural
    interp. represents a year number

    Month is Natural[1,12]
    interp. represents a month number

    Day is Natural[1,31]
    interp. represents a day number

    Date is (Year, Month, Day)
    interp. represents a valid date

    DayOfYear is Natural[1, 365]
    interp. represents a day in the year (e.g. 33 means February 2)
*)


(* ==========================================================================

    Date Date -> bool

    Produces true if first date comes before the second date,
    otherwise produces false.
    - Equal dates produce false as well.

========================================================================== *)
fun is_older (d1: (int * int * int), d2: (int * int * int)) = 
    let
        val d1_year = #1 d1
        val d1_month = #2 d1
        val d1_day = #3 d1

        val d2_year = #1 d2
        val d2_month = #2 d2
        val d2_day = #3 d2
    in
        (d1_year < d2_year) orelse
        (d1_year = d2_year andalso d1_month < d2_month) orelse
        (d1_year = d2_year andalso d1_month = d2_month andalso d1_day < d2_day)
    end

val is_older_test0 = is_older((1, 2, 3), (2, 3, 4)) = true
val is_older_test1 = is_older((2021, 4, 3), (2021, 4, 3)) = false
val is_older_test2 = is_older((2021, 4, 4), (2021, 4, 3)) = false
val is_older_test3 = is_older((2021, 5, 2), (2021, 4, 3)) = false
val is_older_test4 = is_older((2022, 3, 2), (2021, 4, 3)) = false
val is_older_test5 = is_older((2021, 4, 2), (2021, 4, 3)) = true
val is_older_test6 = is_older((2021, 3, 2), (2021, 4, 2)) = true
val is_older_test7 = is_older((2020, 4, 2), (2021, 4, 2)) = true


(* ==========================================================================

    (listof Date) Month -> int

    Return how many dates in the given list are in the given month.

========================================================================== *)
fun number_in_month (dates: (int * int * int) list, month: int) = 
    if null dates
    then 0
    else
        if #2 (hd dates) = month
        then 1 + number_in_month(tl dates, month)
        else number_in_month(tl dates, month)

val number_in_month_test1 = number_in_month([], 2) = 0
val number_in_month_test2 = number_in_month([(2012, 1, 28)], 2) = 0
val number_in_month_test3 = number_in_month([(2012, 2, 28)], 2) = 1
val number_in_month_test4 = number_in_month([(2012, 8, 28), (2013, 12, 1)], 2) = 0
val number_in_month_test5 = number_in_month([(2012, 2, 28), (2013, 12, 1)], 2) = 1
val number_in_month_test6 = number_in_month([(2012, 2, 28), (2013, 2, 1)], 2) = 2


(* ==========================================================================

    (listof Date) (listof Month) -> int

    Return how many dates in the given list are in the given list of months.
    Assume: the list of months has no number repeated.

========================================================================== *)
fun number_in_months (dates: (int * int * int) list, months: int list) =
    if null months orelse null dates
    then 0
    else
        number_in_month(dates, hd months) + number_in_months(dates, tl months)


val number_in_months_test1 = number_in_months([], []) = 0
val number_in_months_test2 = number_in_months([], [1, 2]) = 0
val number_in_months_test3 = number_in_months([(2012,2,28), (2013,12,1)], []) = 0
val number_in_months_test4 = number_in_months([(2012,2,28), (2013,12,1), (2011,3,31), (2011,4,28)], [1]) = 0
val number_in_months_test5 = number_in_months([(2012,2,28), (2013,12,1), (2011,3,31), (2011,4,28)], [2]) = 1
val number_in_months_test6 = number_in_months([(2012,2,28), (2013,12,1), (2011,3,31), (2011,4,28)], [1, 7]) = 0
val number_in_months_test7 = number_in_months([(2012,2,28), (2013,12,1), (2011,3,31), (2011,4,28)], [1, 2]) = 1
val number_in_months_test8 = number_in_months([(2012,2,28), (2013,12,1), (2011,3,31), (2011,4,28)], [2, 4]) = 2
val number_in_months_test9 = number_in_months([(2012,2,28), (2013,12,1), (2011,3,31), (2011,4,28)], [2, 3, 4]) = 3
val number_in_months_test10 = number_in_months([(2012,2,28), (2013,4,1), (2011,3,31), (2011,4,28)], [2, 3, 4]) = 4


(* ==========================================================================

    (listof Date) Month -> (listof Date)

    Returns a list with all dates that are in the given month.

========================================================================== *)
fun dates_in_month (dates: (int * int * int) list, month: int) =
    if null dates
    then []
    else
        if #2 (hd dates) = month
        then (hd dates)::dates_in_month(tl dates, month)
        else dates_in_month(tl dates, month)

val dates_in_month_test1 = dates_in_month([], 2) = []
val dates_in_month_test2 = dates_in_month([(2012,4,28)], 2) = []
val dates_in_month_test3 = dates_in_month([(2012,2,28)], 2) = [(2012,2,28)]
val dates_in_month_test4 = dates_in_month([(2012,2,28), (2013,12,1)], 1) = []
val dates_in_month_test5 = dates_in_month([(2012,2,28), (2013,12,1)], 2) = [(2012,2,28)]
val dates_in_month_test6 = dates_in_month([(2012,12,28), (2013,12,1)], 12) = [(2012,12,28), (2013,12,1)]


(* ==========================================================================

    (listof Date) (listof Month) -> (listof Date)

    Returns a list with all dates that are in the given list of months.

========================================================================== *)
fun dates_in_months (dates: (int * int * int) list, months: int list) =
    if null dates orelse null months
    then []
    else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

val dates_in_months_test1 = dates_in_months([], []) = []
val dates_in_months_test2 = dates_in_months([], [2,3,4]) = []
val dates_in_months_test3 = dates_in_months([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)], []) = []
val dates_in_months_test4 = dates_in_months([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)], [6, 8]) = []
val dates_in_months_test5 = dates_in_months([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)], [2,3,4]) =
    [(2012,2,28), (2011,3,31), (2011,4,28)]
