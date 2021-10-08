(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(*
  String (listof String) -> NONE | SOME (listof String)

  Return NONE if string is not in the list, otherwise return
  the given list without the given string
*)
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


(* 
  (listof (listof String)) String -> (listof String)

  Returns a list with all items in the given list that matches with
  the given string, without insert the given string
*)
fun get_substitutions1 (substitutions, name) =
  case substitutions of
    [] => []
  | namelist::rest => case all_except_option (name, namelist) of
                        NONE => get_substitutions1 (rest, name)
                      | SOME list => list @ get_substitutions1 (rest, name)


(* 
  (listof (listof String)) String -> (listof String)

  Returns a list with all items in the given list that matches with
  the given string, without insert the given string

  Tail-recursive version!
*)
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

(* 
  (listof (listof String)) {first:String, middle:String, last:String}
  ->
  (listof {first:String, middle:String, last:String})

  Returns a list with all the possible full names by substituting for the first name
  using the names on the given list.
*)
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
