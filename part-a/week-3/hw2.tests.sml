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

val card_color_test1 = card_color (Clubs, Num 2) = Black
val card_color_test2 = card_color (Spades, Num 2) = Black
val card_color_test3 = card_color (Diamonds, Num 2) = Red
val card_color_test4 = card_color (Hearts, Num 2) = Red

val card_value_test1 = card_value (Clubs, Num 2) = 2
val card_value_test2 = card_value (Clubs, Num 7) = 7
val card_value_test3 = card_value (Clubs, Ace) = 11
val card_value_test4 = card_value (Clubs, King) = 10
val card_value_test5 = card_value (Clubs, Queen) = 10
val card_value_test6 = card_value (Clubs, Jack) = 10

val remove_card_test1 = remove_card ([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []
val remove_card_test2 = remove_card ([(Hearts, Ace), (Hearts, Ace)], (Hearts, Ace), IllegalMove) = [(Hearts, Ace)]
val remove_card_test3 = (remove_card ([(Hearts, King)], (Hearts, Ace), IllegalMove) handle IllegalMove => []) = []

val all_same_color_test1 = all_same_color [] = true
val all_same_color_test2 = all_same_color [(Hearts, Ace)] = true
val all_same_color_test3 = all_same_color [(Hearts, Ace), (Hearts, Ace)] = true
val all_same_color_test4 = all_same_color [(Hearts, Ace), (Clubs, Ace)] = false
val all_same_color_test5 = all_same_color [(Hearts, Ace), (Diamonds, Ace), (Spades, King)] = false

val sum_cards_test1 = sum_cards [] = 0
val sum_cards_test2 = sum_cards [(Clubs, Num 2),(Clubs, Num 2)] = 4
val sum_cards_test3 = sum_cards [(Clubs, Num 2),(Clubs, Num 2),(Clubs, Ace)] = 15
val sum_cards_test4 = sum_cards [(Clubs, Num 2),(Clubs, Num 2),(Clubs, King)] = 14

val score_test1 = score ([(Hearts, Num 2),(Clubs, Num 4)],10) = 4 (* sum <= goal, different color *)
val score_test2 = score ([(Hearts, Num 2),(Hearts, Num 4)],10) = 2 (* sum <= goal, same color *)
val score_test3 = score ([(Hearts, Num 6),(Clubs, Num 4)],10) = 0 (* sum = goal, different color *)
val score_test4 = score ([(Hearts, Num 6),(Hearts, Num 4)],10) = 0 (* sum = goal, same color *)
val score_test5 = score ([(Hearts, Num 8),(Clubs, Num 4)],10) = 6 (* sum > goal, different color *)
val score_test6 = score ([(Hearts, Num 8),(Hearts, Num 4)],10) = 3 (* sum > goal, same color *)

val officiate_test1 = officiate ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6

val officiate_test2 = officiate ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                        [Draw,Draw,Draw,Draw,Draw],
                        42)
             = 3

val officiate_test3 = ((officiate([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)],
                         42);
               false) 
              handle IllegalMove => true)
