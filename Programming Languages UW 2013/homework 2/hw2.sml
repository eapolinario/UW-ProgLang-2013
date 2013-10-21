fun same_string(s1 : string, s2 : string) =
    s1 = s2

fun all_except_option (s : string, sl : string list) =
    case sl of
	[] => NONE
     | (x::xs) => case same_string (x, s) of
		      true => SOME xs
		   | false => case all_except_option (s, xs) of
				  NONE => NONE
			       | SOME xs' => SOME (x::xs')

fun get_substitutions1 (subs : string list list, s : string) =
    case subs of
	[] => []
      | head::tail =>
	let
	    val substitute = all_except_option (s, head)
	    val tail_substitutions = get_substitutions1 (tail, s)
	in
	    case substitute of
		NONE => tail_substitutions
	      | SOME match => if head = match
			      then tail_substitutions
			      else match @ tail_substitutions
	end

fun get_substitutions2 (subs : string list list, s : string) =
    let
	fun aux (subs, s, acc) =
	    case subs of
		[] => acc
	      | head::tail => case all_except_option (s, head) of
				  NONE => aux (tail, s, acc)
				| SOME match => if head = match
						then aux (tail, s, acc)
						else aux (tail, s, match @ acc)
    in
	aux (subs, s, [])
    end


fun similar_names (lst : string list list, name : { first : string, middle : string, last : string }) =
  let
      fun name_builder (first_names, middle_name, last_name, acc) =
	  case first_names of
              []=> acc
	   |  x::xs => name_builder(xs, middle_name, last_name,  {first=x, middle=middle_name, last=last_name} :: acc)
      val {first=first, middle=middle, last=last} = name
  in
      name_builder(get_substitutions2(lst, first) @ [first], middle, last, [])
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
fun card_color (c : card) =
    let val (s, r) = c
    in
	case s of
	    Spades => Black
	 |  Clubs => Black
	 | Diamonds => Red
	 | Hearts  => Red
    end

fun card_value (c : card) =
    let
	val (s, r) = c
    in
	case r of
	    Ace => 11
	 | Num n => n
	 | _ => 10
    end

fun remove_card (cs : card list, c : card, e : exn) =
    case cs of
	[] => raise e
     | x::xs => if x = c
		then xs
		else remove_card (xs, c, e)

fun all_same_color ( cs : card list ) =
    let fun aux (cs : card list, clr : color)=
	    case cs of
		[] => true
             |  x::xs => case clr = card_color(x) of
			     true => aux(xs, clr)
			  |  false => false
    in
	case cs of
            [] => true
	 |  x::xs => aux(xs, card_color(x))
    end

fun sum_cards ( cs : card list )=
    let fun aux ( cs, acc ) =
	    case cs of
		[] => acc
	     |  x::xs => aux (xs, card_value(x) + acc)
    in
	aux(cs, 0)
    end

fun score (cs : card list, goal : int) =
    let fun pre_score(sum : int, goal : int) =
	    case sum > goal of
		true => 3*(sum-goal)
	     |  false => goal - sum
	fun final_score(cs : card list, pre : int) =
	    case all_same_color ( cs ) of
		true => pre div 2
             |  false => pre
    in
	final_score(cs, pre_score(sum_cards(cs), goal))
    end

fun officiate ( sequence ) =
    let fun loop ( sequence ) =
	    case sequence of
		(_, [], goal, held) => score(held, goal)
	     |  ([], Draw::mvs, goal, held) => score(held, goal)
	     |  (crds, Discard crd::mvs, goal, held) => loop(crds, mvs, goal, remove_card(held, crd, IllegalMove))
	     |  (crd::crds, Draw::mvs, goal, held) =>
		case (card_value(crd) + sum_cards(held)) > goal of
                    true => score(crd::held, goal)
		 |  false => loop(crds, mvs, goal, crd::held)
    in
	case sequence of
            (crds, mvs, goal) => loop (crds,mvs, goal,[])
    end
