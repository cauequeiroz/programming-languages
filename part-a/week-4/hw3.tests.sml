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

val count_wildcards_test1 = count_wildcards Wildcard = 1

val count_wild_and_variable_lengths_test1 = count_wild_and_variable_lengths (Variable("a")) = 1

val count_some_var_test1 = count_some_var ("x", Variable("x")) = 1

val check_pat_test1 = check_pat (Variable("x")) = true
val check_pat_test2 = check_pat (ConstructorP("x", (TupleP [Wildcard, Variable "x", Wildcard]))) = true
val check_pat_test3 = check_pat (ConstructorP("x", (TupleP [Wildcard, Variable "x", Wildcard, Variable "x"]))) = false
val check_pat_test4 = check_pat (TupleP [Variable "a", Wildcard, Variable "c0", ConstructorP ("c1", Variable "c0")]) = false
val check_pat_test5 = check_pat (TupleP [Variable "a", Wildcard, Variable "c0", ConstructorP ("c1", Variable "c1")]) = true


(* val match_test1 = match (Const(1), UnitP) = NONE
val match_test2 = 
  match (Tuple [Const 3, Unit, Constructor ("c0", Const 3), Constructor ("c1", Const 3)],
         TupleP [Variable "a", Wildcard, Variable "c0", ConstructorP ("c1", Variable "c1")]) = SOME [("c1",Const 3),("c0",Constructor ("c0",Const 3)),("a",Const 3)]

val first_match_test1 = first_match Unit [UnitP] = SOME []

val match_testX = match (Const 17,ConstP 17) = SOME([])
val match_testX = match (Unit,Wildcard) = SOME([])
val match_testX = match (Constructor ("egg",Const 4),ConstructorP ("egg",ConstP 4)) = SOME([])
val match_testX = match (Constructor ("egg",Constructor ("egg",Const 4)),ConstructorP ("egg",ConstructorP ("egg",ConstP 4))) = SOME([])
val match_testX = match (Tuple[Const 17,Unit,Const 4,Constructor ("egg",Const 4),Constructor ("egg",Constructor ("egg",Const 4)),Tuple[Const 17,Unit,Const 4,Constructor ("egg",Const 4),Constructor ("egg",Constructor ("egg",Const 4))],Tuple[Unit,Unit],Tuple[Const 17,Const 4],Tuple[Constructor ("egg",Const 4),Constructor ("egg",Const 4)]],TupleP[ConstP 17,Wildcard,ConstP 4,ConstructorP ("egg",ConstP 4),ConstructorP ("egg",ConstructorP ("egg",ConstP 4)),TupleP[ConstP 17,Wildcard,ConstP 4,ConstructorP ("egg",ConstP 4),ConstructorP ("egg",ConstructorP ("egg",ConstP 4))],TupleP[Wildcard,Wildcard],TupleP[ConstP 17,ConstP 4],TupleP[ConstructorP ("egg",ConstP 4),ConstructorP ("egg",ConstP 4)]]) = SOME([]) *)
