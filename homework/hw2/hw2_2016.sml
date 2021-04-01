fun remove_option(str, str_list) = 
   let
      fun same_string(s1 : string, s2 : string) =
         s1 = s2
      fun aux(str_list, acc) = 
         case str_list of
            [] => NONE
            | s::str_list' =>
               if same_string(s, str)
               then SOME (acc @ str_list')
               else aux(str_list', acc @ [s])
   in aux(str_list, [])
   end

fun all_substitutions1(subs, s) = 
   case subs of
      [] => []
      | sub_list::subs' =>
         case remove_option(s, sub_list) of
            SOME str_list => str_list @ all_substitutions1(subs', s)
            | NONE => all_substitutions1(subs', s)

fun all_substitutions2(substitutions, s) = 
   let
      fun aux(substitutions, acc) = 
         case substitutions of
            [] => acc
            | sub_list::substitutions' =>
               case remove_option(s, sub_list) of
                  SOME str_list => aux(substitutions', (acc @ str_list))
                  | NONE => aux(substitutions', acc)
   in aux(substitutions, [])
   end

fun similar_name(lists, full_name) = 
   let
      val {first=f, middle=m, last=l} = full_name
      val new_list = all_substitutions2(lists, f)
      fun namer(list, full_name) = 
         case list of
            [] => []
            | x::list' =>
            case full_name of
                  full_name => {first=x, middle=m, last=l} :: namer(list', full_name)
   in namer(new_list, full_name)
   end

datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = rank * suit
datatype color = Red | Black
datatype move = Discard of card | Draw 
exception IllegalMove

fun card_color (card) =
   case card of
         (_, Clubs) => Black
      | (_, Spades) => Black
      | (_, Diamonds) => Red
      | (_, Hearts) => Red

fun card_value (card) =
   case card of
      (rank, suit) => rank

fun remove_card (cs, c, e) =
   let
      fun aux(cs, acc) = 
         case cs of
            [] => raise e
            | card::cs' =>
               if c = card
               then (acc @ cs')
               else aux(cs', acc @ [card])
   in aux(cs, [])
   end

fun all_same_color (cs) =
   case cs of
      [] => true
      | c::[] => true
      | c1::(c2::rest) => (card_color(c1) = card_color(c2) andalso all_same_color(c2::rest))

fun sum_cards (cs) = 
   let 
      fun sum (cs, acc) = 
         case cs of
            [] => acc
            | (Jack, _)::cs' => sum(cs', acc + 11)
            | (Queen, _)::cs' => sum(cs', acc + 12)
            | (King, _)::cs' => sum(cs', acc + 13)
            | (Ace, _)::cs' => sum(cs', acc + 11)
            | (Num x, _)::cs' => sum(cs', acc + x)
   in sum (cs, 0)
   end

fun score (held_cards, goal) = 
   let
      val sum = sum_cards(held_cards)
      val preliminary_score = 
         if sum > goal
         then 5 * (sum - goal)
         else goal - sum
   in
      if all_same_color(held_cards)
      then preliminary_score div 2
      else preliminary_score
   end

fun officiate (card_list, move_list, goal) =
   let
      val hand = []
      fun helper (card_list, move_list, hand) =
         case move_list of
            [] => score(hand, goal)
            | m::move_list' =>
               case m of 
                  Discard card =>
                        let val new_hand = remove_card(hand, card, IllegalMove)
                        in 
                           if sum_cards(hand) > goal
                           then score(hand, goal)
                           else helper(card_list, move_list', new_hand)
                        end
                  | Draw => 
                     case card_list of
                        [] => score(hand, goal)
                        | c::card_list' => let val new_hand = c::hand 
                                             in
                                                if sum_cards(hand) > goal
                                                then score(hand, goal)
                                                else helper(card_list', move_list', c::hand)
                                             end
   in helper(card_list, move_list, hand)
   end