type year = int
type month = int
type day = int
type date = (year * month * day)

fun get_year (d : date) : year =
  #1 d

fun get_month (d : date) : month =
  #2 d

fun get_day (d : date) : day =
  #3 d

(* 1. Write a function is_older that takes two dates and evaluates to true or *)
(* false. It evaluates to true if the first argument is a date that comes before *)
(* the second argument. (If the two dates are the same, the result is false.) *)
fun is_older (d1 : date, d2 : date) =
  let
    fun lazy_time (d : date) =
      (get_year d * 365) + (get_month d * 31) + (get_day d)
  in
  lazy_time d1 < lazy_time d2
  end

(* 2. Write a function number_in_month that takes a list of dates and a month *)
(* (i.e., an int) and returns how many dates in the list are in the given month. *)
fun number_in_month (ds : date list, m : month) =
  let
    fun process_dates(ds : date list) =
      if null ds then
        0
      else
        (if get_month(hd ds) = m then 1 else 0) + process_dates(tl ds)
  in
  process_dates(ds)
  end

(* 3. Write a function number_in_months that takes a list of dates and a list of *)
(* months (i.e., an int list) and returns the number of dates in the list of dates *)
(* that are in any of the months in the list of months.  Assume the list of months *)
(* has no number repeated. Hint: Use your answer to the previous problem. *)
fun number_in_months (ds : date list, ms : month list) =
  if null ms then
    0
  else
    number_in_month(ds, hd ms) + number_in_months(ds, tl ms)

(* 4. Write a function dates_in_month that takes a list of dates and a month *)
(* (i.e., an int) and returns a list holding the dates from the argument list of *)
(* dates that are in the month. The returned list should contain dates in the *)
(* order they were originally given. *)
fun dates_in_month (ds : date list, m : month) =
  if null ds then
    []
  else if get_month(hd ds) = m then
    hd ds :: dates_in_month(tl ds, m)
  else
    dates_in_month(tl ds, m)

(* 5. Write a function dates_in_months that takes a list of dates and a list of *)
(* months (i.e., an int list) and returns a list holding the dates from the *)
(* argument list of dates that are in any of the months in the list of months. *)
(* Assume the list of months has no number repeated. Hint: Use your answer to the *)
(* previous problem and SML’s list-append operator (@). *)
fun dates_in_months (ds : date list, ms : month list) =
  if null ms then
    []
  else
    dates_in_month(ds, hd ms) :: dates_in_months(ds, tl ms)

(* 6. Write a function get_nth that takes a list of strings and an int n and *)
(* returns the n th element of the list where the head of the list is 1st. Do not *)
(* worry about the case where the list has too few elements: your function may *)
(* apply hd or tl to the empty list in this case, which is okay. *)

(* 7. Write a function date_to_string that takes a date and returns a string of *)
(* the form January 20, 2013 (for example). Use the operator ^ for concatenating *)
(* strings and the library function Int.toString for converting an int to a *)
(* string. For producing the month part, do not use a bunch of conditionals. *)
(* Instead, use a list holding 12 strings and your answer to the previous problem. *)
(* For consistency, put a comma following the day and use capitalized English *)
(* month names: January, February, March, April, May, June, July, August, *)
(* September, October, November, December. *)

(* 8. Write a function number_before_reaching_sum that takes an int called sum, *)
(* which you can assume is positive, and an int list, which you can assume *)
(* contains all positive numbers, and returns an int.  You should return an int n *)
(* such that the first n elements of the list add to less than sum, but the first *)
(* n + 1 elements of the list add to sum or more. Assume the entire list sums to *)
(* more than the passed in value; it is okay for an exception to occur if this is *)
(* not the case. *)

(* 9. Write a function what_month that takes a day of year (i.e., an int between 1 *)
(* and 365) and returns what month that day is in (1 for January, 2 for February, *)
(* etc.). Use a list holding 12 integers and your answer to the previous problem. *)

(* 10. Write a function month_range that takes two days of the year day1 and day2 *)
(* and returns an int list [m1,m2,...,mn] where m1 is the month of day1, m2 is the *)
(* month of day1+1, ..., and mn is the month of day day2. Note the result will *)
(* have length day2 - day1 + 1 or length 0 if day1>day2. *)

(* 11. Write a function oldest that takes a list of dates and evaluates to an *)
(* (int*int*int) option. It evaluates to NONE if the list has no dates and SOME d *)
(* if the date d is the oldest date in the list. *)
