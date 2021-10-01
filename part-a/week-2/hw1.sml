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


(* ==========================================================================

    (listof Date) (listof Month) -> (listof Date)

    Returns a list with all dates that are in the given list of months.

========================================================================== *)
fun dates_in_months (dates: (int * int * int) list, months: int list) =
    if null dates orelse null months
    then []
    else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)


(* ==========================================================================

    (listof string) int -> string

    Returns the element of the given list in the given position.

========================================================================== *)
fun get_nth (elements: string list, position: int) =
    if null elements orelse position = 0
    then ""
    else
        if (position - 1) = 0
        then hd elements
        else get_nth(tl elements, position - 1)


(* ==========================================================================

    Date -> string

    Returns the element of the given list in the given position.

========================================================================== *)
fun date_to_string (date: int * int * int) =
    get_nth(
        ["January", "February", "March", "April",
        "May", "June", "July", "August", "September",
        "October", "November", "December"], #2 date) ^
    " " ^
    Int.toString (#3 date) ^
    ", " ^
    Int.toString (#1 date)


(* ==========================================================================

    int (listof int) -> int

    Returns how many itens in the given list add to less than sum.

========================================================================== *)
fun number_before_reaching_sum (sum: int, numbers: int list) = 
    let
        fun count(counter:int, acc: int, numbers: int list) =
            if null numbers
            then counter
            else
                if (acc + hd numbers) >= sum
                then counter
                else count(counter + 1, acc + hd numbers, tl numbers)
    in
        count(0, 0, numbers)
    end


(* ==========================================================================

    DayOfYear -> int

    Returns in what month number the given day of the year is in.

========================================================================== *)
fun what_month(day: int) =
    let
        val days_by_month = [31,28,31,30,31,30,31,31,30,31,30,31]
    in
        number_before_reaching_sum(day, days_by_month) + 1
    end


(* ==========================================================================

    DayOfYear DayOfYear -> (listof int)

    Returns a list with month of each day between the range of the two given
    days, including both.
    - If the first day is greater than second one, returns an empty list

========================================================================== *)
fun month_range(day1: int, day2: int) =
    if day1 > day2
    then []
    else what_month(day1)::month_range(day1 + 1, day2)


(* ==========================================================================

    (listof Date) -> Date option

    Returns NONE if list has no dates or SOME Date for the oldest
    date in the list.

========================================================================== *)
fun oldest(dates: (int * int * int) list) = 
    if null dates
    then NONE
    else
        let
            fun oldest(dates: (int * int * int) list) =
                if null (tl dates)
                then hd dates
                else 
                    let
                        val oldest_from_tail = oldest(tl dates)
                    in
                        if is_older(hd dates, oldest_from_tail)
                        then hd dates
                        else oldest_from_tail
                    end

        in
            SOME (oldest dates)
        end