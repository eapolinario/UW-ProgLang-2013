fun is_older (date1 : int*int*int, date2 : int*int*int) =
    let
	val y1 = (#1 date1)
	val y2 = (#1 date2)
	val m1 = (#2 date1)
	val m2 = (#2 date2)
	val d1 = (#3 date1)
	val d2 = (#3 date2)
    in
	if y1 < y2
	then true
	else if y1 > y2
	then false
	else if m1 < m2
	then true
	else if m2 > m1
	then false
	else if d1 < d2
	then true
	else if d2 > d1
	then false
	else false
    end

fun number_in_month (xs : (int*int*int) list, month : int) =
    if null xs
    then 0
    else if (#2 (hd xs)) = month
    then 1 + number_in_month(tl xs, month)
    else number_in_month(tl xs, month)

fun number_in_months (xs : (int*int*int) list, months : int list) =
    if null months
    then 0
    else number_in_month (xs, hd months) + number_in_months(xs, tl months)

fun dates_in_month (dates : (int * int * int) list, month : int) =
    if null dates
    then []
    else
	let val hd_m = (hd dates) in
	    if (#2 hd_m) = month
	    then hd_m :: dates_in_month (tl dates, month)
	    else dates_in_month (tl dates, month)
	end

fun dates_in_months (dates : (int * int * int) list, months : int list) =
    if null months
    then []
    else dates_in_month (dates, hd months) @ dates_in_months (dates, tl months)

fun get_nth (xs : string list, n : int) =
    if n = 1
    then hd xs
    else get_nth (tl xs, n - 1)

fun date_to_string (date : int * int * int) =
    let val dates = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in
	 (get_nth (dates, (#2 date))) ^ " " ^(Int.toString (#3 date)) ^ ", " ^ (Int.toString (#1 date))
    end

fun number_before_reaching_sum (sum : int, xs : int list) =
    let fun partial_sum (sum : int, xs : int list, p_sum : int, i : int) =
	if p_sum + (hd xs) >= sum
	then i
	else partial_sum(sum, tl xs, p_sum + (hd xs), i + 1)
    in
	partial_sum (sum, xs, 0, 0)
    end

fun what_month (day : int) =
    let
	val month_durations = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
	number_before_reaching_sum (day, month_durations) + 1
    end

fun month_range (day1 : int, day2 : int) =
    if day1 > day2
    then []
    else
	let
	    fun build_range (from : int, to : int) =
		if from = to
		then [from]
		else from :: build_range (from + 1, to)
	    fun what_months (days : int list) =
		if null days
		then []
		else what_month (hd days) :: what_months (tl days)
	in
	    what_months (build_range (day1, day2))
	end

fun oldest (xs : (int * int * int) list) =
    if null xs
    then NONE
    else
	let
	    val old = hd xs
	    fun compare_old_dates ( xs : (int * int * int) list, partial : (int * int * int)) =
		if null xs
		then partial
		else
		    let
			val current = hd xs
		    in
			if is_older(partial, current)
			then compare_old_dates(tl xs, partial)
			else compare_old_dates(tl xs, current)
		    end
	in
	    SOME (compare_old_dates(tl xs, hd xs) )
	end
