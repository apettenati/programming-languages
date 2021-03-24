(* You will write 12 SML functions (and tests for them) related to calendar dates. In all problems, a “date” is
an SML value of type int*int*int, where the first part is the day, the second part is the month, and the
third part is the year. A “reasonable” date has a positive year, a month between 1 and 12, and a day no
greater than 31 (or less depending on the month). Your solutions need to work correctly only for reasonable
dates, but do not check for reasonable dates (that is a challenge problem) and many of your functions will
naturally work correctly for some/all non-reasonable dates. A “day of year” is a number from 1 to 365
where, for example, 33 represents February 2. (We ignore leap years except in one challenge problem.) *)

(* 1. Write a function is_older that takes two dates and evaluates to true or false. It evaluates to true if
the first argument is a date that comes before the second argument. (If the two dates are the same,
the result is false.) *)
fun is_older (x : int*int*int, y : int*int*int) = 
  if x = y
  then false
  else #3 x < #3 y orelse #2 x < #2 y orelse #1 x < #1 y

(* 2. Write a function number_in_month that takes a list of dates and a month (i.e., an int) and returns
how many dates in the list are in the given month. *)

fun number_in_month (xs : (int*int*int) list, y : int) = 
  if null xs
  then 0
  else (
    (
      if y = #2 (hd xs)
      then 1
      else 0
    )
    + number_in_month(tl xs, y)
  )

(* 3. Write a function number_in_months that takes a list of dates and a list of months (i.e., an int list)
and returns the number of dates in the list of dates that are in any of the months in the list of months.
Assume the list of months has no number repeated. Hint: Use your answer to the previous problem. *)

fun number_in_months (xs : (int*int*int) list, ys: int list) =
  if null xs orelse null ys
  then 0
  else number_in_month(xs, hd ys) + number_in_months(xs, tl ys)

(* 4. Write a function dates_in_month that takes a list of dates and a month (i.e., an int) and returns a
list holding the dates from the argument list of dates that are in the month. The returned list should
contain dates in the order they were originally given. *)

fun dates_in_month (date_list: (int*int*int) list, month: int) =
  if null date_list
  then []
  else (
    let 
      val date_list_item = hd date_list
      val date_list_item_month = #2 date_list_item
    in 
      (if date_list_item_month = month then [date_list_item] else [] ) @ dates_in_month((tl date_list), month)
    end
  )

(* 5. Write a function dates_in_months that takes a list of dates and a list of months (i.e., an int list)
and returns a list holding the dates from the argument list of dates that are in any of the months in
the list of months. Assume the list of months has no number repeated. Hint: Use your answer to the
previous problem and SML’s list-append operator (@). *)

fun dates_in_months (date_list : (int*int*int) list, month_list: int list) = 
  if null date_list orelse null month_list
  then []
  else dates_in_month(date_list, hd month_list) @ dates_in_months(date_list, tl month_list) 


(* 6. Write a function get_nth that takes a list of strings and an int n and returns the n th element of the
list where the head of the list is 1 st . Do not worry about the case where the list has too few elements:
your function may apply hd or tl to the empty list in this case, which is okay. *)

fun get_nth(x: string list, n: int) = 
  if n = 1
  then hd x
  else get_nth(tl x, n - 1)

(* 7. Write a function date_to_string that takes a date and returns a string of the form September-10-2015
(for example). Use the operator ^ for concatenating strings and the library function Int.toString
for converting an int to a string. For producing the month part, do not use a bunch of conditionals.
Instead, use a list holding 12 strings and your answer to the previous problem. For consistency, use
hyphens exactly as in the example and use English month names: January, February, March, April,
May, June, July, August, September, October, November, December. *)

fun date_to_string(date: int*int*int) = 
  let
    val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    val month = get_nth(months, #2 date)
    val day = Int.toString(#1 date)
    val year = Int.toString(#3 date)
  in 
    month^"-"^day^"-"^year
  end

(* 8. Write a function number_before_reaching_sum that takes an int called sum, which you can assume
is positive, and an int list, which you can assume contains all positive numbers, and returns an int.
You should return an int n such that the first n elements of the list add to less than sum, but the first
n + 1 elements of the list add to sum or more. Assume the entire list sums to more than the passed in
value; it is okay for an exception to occur if this is not the case. *)

fun number_before_reaching_sum (sum: int, int_list: int list) =
  let 
    val count = 0
    fun counter(sum: int, int_list: int list, count: int) = 
      if (sum - hd int_list) < 1
      then count
      else counter(sum - hd int_list, tl int_list, count + 1)
  in counter(sum, int_list, count)
  end

(* 9. Write a function what_month that takes a day of year (i.e., an int between 1 and 365) and returns
what month that day is in (1 for January, 2 for February, etc.). Use a list holding 12 integers and your
answer to the previous problem. *)

fun what_month (day_of_year: int) = 
  let 
    val month_day_count = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  in number_before_reaching_sum (day_of_year, month_day_count) + 1
  end


(* 10. Write a function month_range that takes two days of the year day1 and day2 and returns an int list
[m1,m2,...,mn] where m1 is the month of day1, m2 is the month of day1+1, ..., and mn is the month
of day day2. Note the result will have length day2 - day1 + 1 or length 0 if day1>day2. *)

fun month_range(day1: int, day2: int) = 
  if day2 < day1
  then []
  else
    if day1 = day2
    then [what_month(day2)]
    else what_month(day1) :: month_range(day1 + 1, day2)

(* 11. Write a function oldest that takes a list of dates and evaluates to an (int*int*int) option. It
evaluates to NONE if the list has no dates else SOME d where the date d is the oldest date in the list. *)

fun get_oldest_date (date1: int*int*int, date2: int*int*int) = 
  if is_older(date1, date2)
  then date1
  else date2

fun oldest (xs : (int*int*int) list) = 
  if null xs
  then NONE
  else (
    if length xs = 1
    then SOME (hd xs)
    else 
      let 
        val oldest_date = get_oldest_date(hd xs, hd (tl xs))
        val new_list = oldest_date :: tl (tl xs)
      in oldest(new_list)
      end
  )

(* 12. Write a function cumulative_sum that takes a list of numbers and returns a list of the partial sums
of these numbers. For example, cumulative_sum [12,27,13] = [12,39,52]. Hint: Use a helper
function that takes two arguments. *)

fun increaser (remaining : int list, cumm_sum : int) = 
  if null remaining
  then []
  else (
    let 
      val new_cumm = cumm_sum + hd remaining
      in new_cumm :: increaser(tl remaining, new_cumm)
    end
  )

fun cumulative_sum (x: int list) = 
  increaser(x, 0)


(* 13. Challenge Problem: Write functions number_in_months_challenge and dates_in_months_challenge
that are like your solutions to problems 3 and 5 except having a month in the second argument multiple
times has no more effect than having it once. (Hint: Remove duplicates, then use previous work.) *)


(* 14. Challenge Problem: Write a function reasonable_date that takes a date and determines if it
describes a real date in the common era. A “real date” has a positive year (year 0 did not exist), a
month between 1 and 12, and a day appropriate for the month. Solutions should properly handle leap
years. Leap years are years that are either divisible by 400 or divisible by 4 but not divisible by 100.
(Do not worry about days possibly lost in the conversion to the Gregorian calendar in the Late 1500s.) *)