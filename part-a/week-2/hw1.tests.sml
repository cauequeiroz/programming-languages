use "hw1.sml";

val is_older_test0 = is_older((1, 2, 3), (2, 3, 4)) = true
val is_older_test1 = is_older((2021, 4, 3), (2021, 4, 3)) = false
val is_older_test2 = is_older((2021, 4, 4), (2021, 4, 3)) = false
val is_older_test3 = is_older((2021, 5, 2), (2021, 4, 3)) = false
val is_older_test4 = is_older((2022, 3, 2), (2021, 4, 3)) = false
val is_older_test5 = is_older((2021, 4, 2), (2021, 4, 3)) = true
val is_older_test6 = is_older((2021, 3, 2), (2021, 4, 2)) = true
val is_older_test7 = is_older((2020, 4, 2), (2021, 4, 2)) = true

val number_in_month_test1 = number_in_month([], 2) = 0
val number_in_month_test2 = number_in_month([(2012, 1, 28)], 2) = 0
val number_in_month_test3 = number_in_month([(2012, 2, 28)], 2) = 1
val number_in_month_test4 = number_in_month([(2012, 8, 28), (2013, 12, 1)], 2) = 0
val number_in_month_test5 = number_in_month([(2012, 2, 28), (2013, 12, 1)], 2) = 1
val number_in_month_test6 = number_in_month([(2012, 2, 28), (2013, 2, 1)], 2) = 2

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

val dates_in_month_test1 = dates_in_month([], 2) = []
val dates_in_month_test2 = dates_in_month([(2012,4,28)], 2) = []
val dates_in_month_test3 = dates_in_month([(2012,2,28)], 2) = [(2012,2,28)]
val dates_in_month_test4 = dates_in_month([(2012,2,28), (2013,12,1)], 1) = []
val dates_in_month_test5 = dates_in_month([(2012,2,28), (2013,12,1)], 2) = [(2012,2,28)]
val dates_in_month_test6 = dates_in_month([(2012,12,28), (2013,12,1)], 12) = [(2012,12,28), (2013,12,1)]

val dates_in_months_test1 = dates_in_months([], []) = []
val dates_in_months_test2 = dates_in_months([], [2,3,4]) = []
val dates_in_months_test3 = dates_in_months([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)], []) = []
val dates_in_months_test4 = dates_in_months([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)], [6, 8]) = []
val dates_in_months_test5 = dates_in_months([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)], [2,3,4]) =
    [(2012,2,28), (2011,3,31), (2011,4,28)]

val get_nth_test1 = get_nth([], 1) = ""
val get_nth_test2 = get_nth(["hi", "there", "how", "are", "you"], 0) = ""
val get_nth_test3 = get_nth(["hi", "there", "how", "are", "you"], 1) = "hi"
val get_nth_test4 = get_nth(["hi", "there", "how", "are", "you"], 2) = "there"
val get_nth_test5 = get_nth(["hi", "there", "how", "are", "you"], 3) = "how"

val date_to_string_test1 = date_to_string(2021, 3, 2) = "March 2, 2021"
val date_to_string_test2 = date_to_string(2013, 6, 1) = "June 1, 2013"
val date_to_string_test3 = date_to_string(1500, 12, 6) = "December 6, 1500"

val number_before_reaching_sum_test1 = number_before_reaching_sum(1, [1,2]) = 0
val number_before_reaching_sum_test2 = number_before_reaching_sum(2, [1,2]) = 1
val number_before_reaching_sum_test3 = number_before_reaching_sum(12, [1,2,3,4,5]) = 4
val number_before_reaching_sum_test4 = number_before_reaching_sum(10, [1,2,3,4,5]) = 3
val number_before_reaching_sum_test5 = number_before_reaching_sum(0, [1,2,3,4,5]) = 0

val what_month_test1 = what_month 1 = 1
val what_month_test2 = what_month 31 = 1
val what_month_test3 = what_month 32 = 2
val what_month_test4 = what_month 59 = 2
val what_month_test5 = what_month 60 = 3
val what_month_test6 = what_month 70 = 3
val what_month_test7 = what_month 365 = 12

val month_range_test1 = month_range(2, 1) = []
val month_range_test2 = month_range(30, 33) = [1,1,2,2]
val month_range_test3 = month_range(31, 34) = [1,2,2,2]

val oldest_test1 = oldest([]) = NONE
val oldest_test2 = oldest([(2011,3,31),(2011,3,31),(2011,3,31)]) = SOME (2011,3,31)
val oldest_test3 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31)