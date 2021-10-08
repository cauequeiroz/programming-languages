(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option (str, strlist) =
  let
    fun filter_list list =
      case list of
        [] => []
      | x::xs => if same_string (x, str)
                 then filter_list xs
                 else x::filter_list xs
    
    val filtered_list = filter_list strlist
  in
    if filtered_list = strlist
    then NONE
    else SOME filtered_list
  end


fun get_substitutions1 (substitutions, name) =
  case substitutions of
    [] => []
  | namelist::rest => case all_except_option (name, namelist) of
                        NONE => get_substitutions1 (rest, name)
                      | SOME list => list @ get_substitutions1 (rest, name)


fun get_substitutions2 (substitutions, name) =
  let
    fun mount_list (list, acc) =
      case list of
        [] => acc
      | namelist::rest => case all_except_option (name, namelist) of
                            NONE => mount_list (rest, acc)
                          | SOME list => mount_list (rest, acc @ list)
  in
    mount_list(substitutions, [])
  end


fun similar_names (substitutions, {first=fname, middle=mname, last=lname}) =
  let
    val names = get_substitutions2(substitutions, fname)

    fun mount_list (list, acc) =
      case list of
        [] => acc
      | name::rest => mount_list(rest, acc @ [{first=name, middle=mname, last=lname}])
  in
    mount_list(names, [{first=fname, middle=mname, last=lname}])
  end


(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

fun card_color (suit, rank) = 
  case suit of
    Clubs => Black
  | Spades => Black
  | _ => Red


fun card_value (suit, rank) = 
  case rank of
    Num i => i
  | Ace => 11
  | _ => 10

(*
  Removes card from list of cards
  - remove only the first occurency
  - raise exception if card is not on the list
*)
fun remove_card (cardlist, card, err) =
  let
    fun filter_list (list, acc) =
      case list of 
        [] => acc
      | x::xs => if x = card
                 then acc @ xs
                 else filter_list(xs, acc @ [x])

    val filtered_list = filter_list(cardlist, [])
  in
    if filtered_list = cardlist
    then raise err
    else filtered_list
  end


fun all_same_color cardlist =
  let
    fun check_color (list, color) =
      case list of
        [] => true
      | x::xs => if card_color x <> color then false else check_color(xs, color)
  in
    case cardlist of 
      [] => true
    | x::xs => check_color(xs, card_color x)
  end


fun sum_cards cardlist = 
  let
    fun sum_all (list, acc) =
      case list of 
        [] => acc
      | x::xs => sum_all(xs, acc + card_value x)
  in
    sum_all(cardlist, 0)
  end


