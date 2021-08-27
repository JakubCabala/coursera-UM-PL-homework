fun is_older(x : int*int*int, y: int*int*int) = 
    if #1 x = #1 y  andalso #2 x = #2 y 
    then if #3 x < #3 y then true else false
    else if #1 x = #1 y 
    then if #2 x < #2 y then true else false
    else if #1 x < #1 y then true else false

fun number_in_month(dates: (int*int*int) list, month) = 
    if null dates then 0
    else if #2 (hd dates) = month then 1 + number_in_month(tl dates, month)
    else number_in_month(tl dates, month)

fun number_in_months(dates : (int*int*int) list, months : int list) = 
    if null months then 0
    else number_in_month(dates, hd months) + number_in_months(dates, tl months)

fun dates_in_month (dates : (int*int*int) list, month : int) = 
    if null dates then [] 
    else if #2 (hd dates) = month 
    then (hd dates)::dates_in_month(tl dates, month)
    else dates_in_month(tl dates, month)

fun dates_in_months (dates : (int*int*int) list, months : int list) = 
    if null months then [] 
    else dates_in_month(dates, hd months)@dates_in_months(dates, tl months)

fun get_nth (xs : string list, n : int) = 
    if n = 1 then hd xs 
    else get_nth(tl xs, n - 1)

fun date_to_string (yr: int, month:int, day: int) =
    let
        val months = ["January", "February", "March", "April","May", "June", "July",
                     "August", "September", "October", "November", "December"]
    in
        get_nth(months, month) ^ " " ^ Int.toString(day) ^ ", " ^ Int.toString(yr)
    end

fun number_before_reaching_sum (sum : int, xs : int list) = 
    if null xs then 0
    else if sum - hd xs > 0 then 1 + number_before_reaching_sum(sum - hd xs, tl xs)
    else 0

fun what_month (day : int) = 
    let
        val days   =   [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

        fun f (d : int, months_days : int list, n : int) = 
            if d - hd months_days > 0 then f (d - hd months_days, tl months_days, n + 1)
            else n

    in
        f (day, days, 1)
    end

fun month_range(a : int, b : int) = 
    if a > b then []
    else what_month(a)::month_range(a + 1, b)

fun oldest(dates: (int*int*int) list) = 
    if null dates
    then NONE
    else
        let
            fun f (dates : (int*int*int) list, oldest_one : int*int*int) = 
                if null dates then SOME oldest_one
                else if is_older(hd dates, oldest_one) then f (tl dates, hd dates)
                else f(tl dates, oldest_one)
        in
            f(tl dates, hd(dates))
        end
