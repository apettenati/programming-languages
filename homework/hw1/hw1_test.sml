use "hw1.sml";
val test1_1 =
  let
    val date1 = (12, 12, 12)
    val date2 = (1, 1, 1)
  in is_older(date1, date2) = false
  end;
val test1_2 =
  let
    val d1 = (15, 6, 19)
    val d2 =(16, 6, 19)
  in is_older(d1, d2) = true
  end;
val test1_3 =
  let
    val d11 = (15, 7, 19)
    val d22 = (15, 6, 19)
  in is_older(d11, d22) = false
  end;
val test1_4 =
  let
    val d11 = (15, 7, 19)
    val d22 = (15, 7, 19)
  in is_older(d11, d22) = false
  end;
val d1 = (15, 7, 19)
val d2 = (15, 8, 19)
val d3 = (15, 7, 19)
val mL = [d1, d2, d3]
val test2_1 = number_in_month(mL, 7) = 2;
val test2_2 = number_in_month(mL, 8) = 1;
val test2_3 = number_in_month(mL, 1) = 0;
val test3_1 = number_in_months(mL, [1 , 2, 3]) = 0;
val test3_2 = number_in_months(mL, [7 , 2, 3]) = 2;
val test3_3 = number_in_months(mL, [7 , 8, 3]) = 3;
val test4_1 = dates_in_month(mL, 1) = [];
val test4_2 = dates_in_month(mL, 7) = [d1, d3];
val test4_3 = dates_in_month(mL, 8) = [d2];
val test5_1 = dates_in_months(mL, [1]) = [];
val test5_2 = dates_in_months(mL, [1, 2 ,3]) = [];
val test5_3 = dates_in_months(mL, [7, 2 ,3]) = [d1, d3];
val test5_4 = dates_in_months(mL, [8, 7 ,3]) = [d2, d1, d3];
val strList = ["a", "b", "c", "d"];
val test6_1 = get_nth (strList, 1) = "a";
val test6_2 = get_nth (strList, 2) = "b";
val test6_3 = get_nth (strList, 3) = "c";
val test6_4 = get_nth (strList, 4) = "d";
val test7_1 = date_to_string ((1,1,1)) = "January-1-1";
val test7_2 = date_to_string (d1) = "July-15-19";
val test7_3 = date_to_string (d2) = "August-15-19";
val test8_1 = number_before_reaching_sum(10, [10, 11]) = 0;
val test8_2 = number_before_reaching_sum(11, [10, 11]) = 1;
val test8_3 = number_before_reaching_sum(3, [1, 1, 1]) = 2;
val test8_4 = number_before_reaching_sum(4, [1, 1, 1, 1]) = 3;
val test9_1 = what_month (2) = 1;
val test9_2 = what_month (31) = 1;
val test9_3 = what_month (32) = 2;
val test9_4 = what_month (360) = 12;
val test9_5 = what_month (334) = 11;
val test10_1 = month_range (1, 2) = [1, 1];
val test10_2 = month_range (31, 33) = [1, 2, 2];
val test10_3 = month_range (334, 337) = [11, 12, 12, 12];
val test10_4 = month_range (2, 1) = [];
val test11_1 = oldest (mL) = SOME d1;
val test11_2 = oldest ((1, 1, 28) :: mL) = SOME (1, 1, 28);
val test11_3 = oldest ([]) = NONE;
val test12_1 = cumulative_sum ([12, 27, 13]) = [12, 39, 52];