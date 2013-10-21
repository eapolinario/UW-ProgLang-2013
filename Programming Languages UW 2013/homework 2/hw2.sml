fun same_string(s1 : string, s2 : string) =
    s1 = s2

fun all_except_option (s : string, sl : string list) =
    case sl of
	[] => SOME []
     | (x::xs) => let val rest = all_except_option (s, xs) in
		      case rest of
			  NONE => NONE
		       |  SOME xs' => if same_string (x, s)
				      then SOME xs'
				      else SOME (x::xs')
		  end

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
