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
		NONE => tail_substitutions1
	      | SOME match => if head = match
			      then tail_substitutions
			      else match @ tail_substitutions
	end
