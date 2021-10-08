use "hw2.sml";

val all_except_option_test1 = all_except_option ("notfound", ["string"]) = NONE
val all_except_option_test2 = all_except_option ("string", ["string"]) = SOME []
val all_except_option_test3 = all_except_option ("caue", ["jessica", "caue", "carol"]) = SOME ["jessica", "carol"]

val get_substitutions1_test1 = get_substitutions1 ([], "foo") = []
val get_substitutions1_test2 = get_substitutions1 ([["bar"],["there"]], "foo") = []
val get_substitutions1_test3 = get_substitutions1 ([["foo"],["there"]], "foo") = []
val get_substitutions1_test4 = get_substitutions1 ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Fred") = ["Fredrick","Freddie","F"]
val get_substitutions1_test5 = get_substitutions1 ([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]], "Jeff") = ["Jeffrey","Geoff","Jeffrey"]

val get_substitutions2_test1 = get_substitutions2 ([], "foo") = []
val get_substitutions2_test2 = get_substitutions2 ([["bar"],["there"]], "foo") = []
val get_substitutions2_test3 = get_substitutions2 ([["foo"],["there"]], "foo") = []
val get_substitutions2_test4 = get_substitutions2 ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Fred") = ["Fredrick","Freddie","F"]
val get_substitutions2_test5 = get_substitutions2 ([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]], "Jeff") = ["Jeffrey","Geoff","Jeffrey"]

val similar_names_test1 = similar_names ([], {first="Jeff", middle="W", last="Smith"}) =
	    [{first="Jeff", middle="W", last="Smith"}]
val similar_names_test2 = similar_names ([["foo", "bar"], ["fozz", "barz"]], {first="Jeff", middle="W", last="Smith"}) =
	    [{first="Jeff", middle="W", last="Smith"}]
val similar_names_test3 = similar_names ([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]], {first="Jeff", middle="W", last="Smith"}) =
	    [{first="Jeff", middle="W", last="Smith"}, {first="Jeffrey", last="Smith", middle="W"},
       {first="Geoff", last="Smith", middle="W"}, {first="Jeffrey", last="Smith", middle="W"}]
val similar_names_test4 = similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) =
	    [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"},
	     {first="Freddie", last="Smith", middle="W"}, {first="F", last="Smith", middle="W"}]
