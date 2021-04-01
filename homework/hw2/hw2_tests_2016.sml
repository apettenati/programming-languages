use "hw2_2016.sml";
val str_item = "test";
val str_includes = ["a", "number", "test", "silly"];
val str_excludes = ["a", "number", "silly"];
val str_singleton_includes = ["test"];
val str_singleton_excludes = ["silly"];
val str_empty = []: string list;
val test_1a_1 = remove_option(str_item, str_includes) = SOME ["a", "number", "silly"];
val test_1a_2 = remove_option(str_item, str_excludes) = NONE;
val test_1a_3 = remove_option(str_item, str_empty) = NONE;
val test_1a_4 = remove_option(str_item, str_singleton_includes) = SOME [];
val test_1a_5 = remove_option(str_item, str_singleton_excludes) = NONE;
val test_1b_1 = all_substitutions1([["a", "b"], ["b", "c"], ["d", "e"]], "b") = ["a", "c"];
val test_1b_2 = all_substitutions1([["test", "another test"], ["wow test", "so many test"], ["d", "e"]], "b") = [];
val test_1b_3 = all_substitutions1([[], []], "b") = [];
val test_1b_4 = all_substitutions1([], "b") = [];
val test_1b_5 = all_substitutions1([["a", "b"], ["b", "c"], ["d", "e", "b", "f"]], "b") = ["a", "c", "d", "e", "f"];
val test_1c_1 = all_substitutions2([["a", "b"], ["b", "c"], ["d", "e"]], "b") = ["a", "c"];
val test_1c_2 = all_substitutions2([["test", "another test"], ["wow test", "so many test"], ["d", "e"]], "b") = [];
val test_1c_3 = all_substitutions2([[], []], "b") = [];
val test_1c_4 = all_substitutions2([], "b") = [];
val test_1c_5 = all_substitutions2([["a", "b"], ["b", "c"], ["d", "e", "b", "f"]], "b") = ["a", "c", "d", "e", "f"];
val test_1d_1 = similar_name([["a", "b"], ["b", "c"], ["d", "f"]], {first="b", middle="test", last="lasttest"}) = [{first="a",last="lasttest",middle="test"}, {first="c",last="lasttest",middle="test"}];
val test_1d_2 = similar_name([["d", "f"]], {first="b", middle="test", last="lasttest"}) = [];
val test_1d_3 = similar_name([["b"]], {first="b", middle="test", last="lasttest"}) = [];
val test_1d_4 = similar_name([["a"]], {first="b", middle="test", last="lasttest"}) = [];
val test_1d_5 = similar_name([["a", "b"]], {first="b", middle="test", last="lasttest"}) = [{first="a",last="lasttest",middle="test"}];
val c1 = (Num 10, Clubs);
val c2 = (Jack, Hearts);
val c3 = (Ace, Spades);
val c4 = (Ace, Clubs);
val cs = [c2, c3];
exception TestException
val card_color_1 = card_color(c1) = Black;
val card_color_2 = card_color(c2) = Red;
val card_color_3 = card_color(c3) = Black;
val card_value_1 = card_value(c1) = #1 c1;
val card_value_2 = card_value(c2) = Jack;
val card_value_3 = card_value(c3) = Ace;
val remove_card_1 = remove_card(cs, c2, TestException) = [c3];
val remove_card_2 = remove_card(cs, c3, TestException) = [c2];
val remove_card_3 = (remove_card(cs, c4, TestException) handle TestException => cs) = cs;
val all_same_color_1 = all_same_color([c1, c3, c4]) = true;
val all_same_color_2 = all_same_color([c1]) = true;
val all_same_color_3 = all_same_color([]) = true;
val all_same_color_4 = all_same_color([c1, c2, c3, c4]) = false;
val sum_cards_1 = sum_cards([c1, c2, c3, c4]) = 23;
val sum_cards_2 = sum_cards([c2]) = 11;
val sum_cards_3 = sum_cards([]) = 0;
val score_1 = score([c1, c2, c3, c4], 20) = 5*(23-20);
val score_2 = score([c2], 10) = 5;
val score_3 = score([c4], 1) = 1;

fun provided_test1 () = (* correct behavior: raise IllegalMove *)
    let 
        val cards = [(Jack,Clubs),(Num(8),Spades)]
        val moves = [Draw,Discard(Jack,Hearts)]
    in officiate(cards, moves, 42)
    end

fun provided_test2 () = (* correct behavior: return 5 *)
    let val cards = [(Ace,Clubs),(Ace,Spades),(Ace,Clubs),(Ace,Spades)]
	val moves = [Draw,Draw,Draw,Draw,Draw]
    in
 	officiate(cards,moves,42)
    end

val officiate_1 = officiate([], [Draw], 2) = 1;
val officiate_2 = officiate([c1, c3], [Draw, Draw], 8) = 5;
val officiate_3 = officiate([c1, c3], [Draw, Discard(c1)], 8) = 5;