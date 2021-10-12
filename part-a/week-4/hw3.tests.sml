use "hw3.sml";


val only_capitals_test1 = only_capitals [] = []
val only_capitals_test2 = only_capitals ["a", "b", "c"] = []
val only_capitals_test3 = only_capitals ["A","B","C"] = ["A","B","C"]
val only_capitals_test4 = only_capitals ["A","b","C"] = ["A","C"]
val only_capitals_test5 = only_capitals ["Caue", "Jessica", "carol"] = ["Caue", "Jessica"]

val longest_string1_test1 = longest_string1 [] = ""
val longest_string1_test2 = longest_string1 ["A","bc","C"] = "bc"
val longest_string1_test3 = longest_string1 ["A","bc","Ca"] = "bc"
val longest_string1_test4 = longest_string1 ["A","bc","Cav"] = "Cav"

val longest_string2_test1 = longest_string2 [] = ""
val longest_string2_test2 = longest_string2 ["A","bc","C"] = "bc"
val longest_string2_test3 = longest_string2 ["A","bc","Ca"] = "Ca"
val longest_string2_test4 = longest_string2 ["A","bc","Cav"] = "Cav"

val longest_string3_test1 = longest_string3 [] = ""
val longest_string3_test2 = longest_string3 ["A","bc","C"] = "bc"
val longest_string3_test3 = longest_string3 ["A","bc","Ca"] = "bc"
val longest_string3_test4 = longest_string3 ["A","bc","Cav"] = "Cav"

val longest_string4_test1 = longest_string4 [] = ""
val longest_string4_test2 = longest_string4 ["A","bc","C"] = "bc"
val longest_string4_test3 = longest_string4 ["A","bc","Ca"] = "Ca"
val longest_string4_test4 = longest_string4 ["A","B","C"] = "C"
val longest_string4_test5 = longest_string4 ["A","bc","Cav"] = "Cav"

val longest_capitalized_test1 = longest_capitalized [] = ""
val longest_capitalized_test2 = longest_capitalized ["A","bc","C"] = "A"
val longest_capitalized_test3 = longest_capitalized ["a","bc","C"] = "C"

val rev_string_test1 = rev_string "" = ""
val rev_string_test2 = rev_string "abc" = "cba"
val rev_string_test3 = rev_string "Abc" = "cbA"

val first_answer_test1 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4
val first_answer_test2 = ((first_answer (fn x => if x > 5 then SOME x else NONE) [1,2,3,4,5]) handle NoAnswer => 0) = 0

val all_answers_test1 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [] = SOME []
val all_answers_test2 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE
val all_answers_test3 = all_answers (fn x => if x > 1 then SOME [x] else NONE) [2,3,4,5,6,7] = SOME [2,3,4,5,6,7]
