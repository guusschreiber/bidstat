/*  File        profile.pl
 *  Part of     bidstat: analysis of bridge bidding 
 *  Author      Guus Schreiber, guus.schreiber@vu.nl
 *  Works with  SWI-Prolog 7.1.3
 */

/*--------------------------------------------------
 *  Matching a hand and auction profile
 *---------------------------------------------------*/

match_profile(Set, Profile, Subset, Hand, Suit, Features) :-
  once(match_hand(Set, Profile, Hand, Suit, Features)), 
  once(match_auction(Set, Profile, Hand, Subset)). 

match_hand(Set, Profile, Hand, Suit, Features) :-
  nth1(1, Set, ID),
  find_matching_hand(ID, 1, Hand, Profile), 
  find_extra(ID, Profile, Hand, Suit, Features).

find_extra(ID, Profile, Hand, SuitNum, [HCP, SuitQ]) :-
  memberchk(main_suit([SuitName | _]), Profile),
  suit_order(SuitName, SuitNum), 
  hand(ID, Hand, HCP, _, _), 
  suit_quality(ID, Hand, SuitNum, SuitQ). 
find_extra(ID, Profile, Hand, 0, [HCP]) :-
  \+ memberchk(main_suit(_), Profile),
  hand(ID, Hand, HCP, _, _). 

find_matching_hand(_, 5, _, _) :- !, fail.
find_matching_hand(ID, Hand, Hand, Profile) :-
  forall(member(Req, Profile), match_hand_criterion(ID,Hand,Req)).
find_matching_hand(ID, PrevHand, Hand, Profile) :-
  NextHand is PrevHand + 1, 
  find_matching_hand(ID, NextHand, Hand, Profile).

is_hand_req(hcp).
is_hand_req(main_suit).
is_hand_req(suit).
is_hand_req(feature).

match_hand_criterion(_, _, Req) :-
  Req =.. [Type, _], 
  \+ is_hand_req(Type). 
match_hand_criterion(ID, Hand, hcp([Min, Max])) :-
  hcp(ID, Hand, Min-Max). 
match_hand_criterion(ID, Hand, main_suit([Suit, Length, Op])) :-
  suit_length(ID, Hand, Suit, Length, Op). 
match_hand_criterion(ID, Hand, suit([Suit, Length, Op])) :-
  suit_length(ID, Hand, Suit, Length, Op). 
match_hand_criterion(ID, Hand, feature(Reqs)) :-
  hand(ID, Hand, _, _, HandFeatures), 
  memberchk(Feature, Reqs),
  memberchk(Feature, HandFeatures). 

match_auction(Set, _, 1, Set).
match_auction(Set, Profile, Hand, SubSet) :-
  AL is Hand - 1, 
  memberchk(auction(Auctions), Profile),
  findall(A, (member(A, Auctions), length(A, AL)), As), 
  findall(ID, (member(ID, Set), start_auction(ID, As)), SubSet). 

start_auction(ID, AuctionStarts) :-
  deal_feature(ID, auction(Auction)),
  member(AStart, AuctionStarts), 
  start_list(AStart, Auction). 

start_list([E], [E|_]). 
start_list([E | Rest1], [E | Rest2]) :-
  start_list(Rest1, Rest2).

% Hand matching preds

hcp(ID, Hand, Low-High) :-
  hand(ID, Hand, HCP, _, _), 
  HCP >= Low, 
  HCP =< High. 

suit_length(ID, Hand, Suit, Length, Op) :-
  hand(ID, Hand, _, [ExactDistribution, _], _),
  suit_order(Suit, Num),
  nth1(Num, ExactDistribution, SuitLength),  
  check_suit_length(SuitLength, Length, Op).

check_suit_length(SuitLength, Length, =) :-
  SuitLength == Length. 
check_suit_length(SuitLength, Length, +) :-
  SuitLength >= Length. 
check_suit_length(SuitLength, Length, -) :-
  SuitLength =< Length. 

suit_order(spades, 1). 
suit_order(hearts, 2). 
suit_order(diamonds, 3). 
suit_order(clubs, 4).
suit_order(major, 1).
suit_order(major, 2).
suit_order(minor, 3).
suit_order(minor, 4).
suit_order(suit, 1). 
suit_order(suit, 2). 
suit_order(suit, 3). 
suit_order(suit, 4). 

suit_quality(ID, Hand, Suit, QualityScore) :-
  deal_feature(ID, hands(Hands)),
  nth1(Hand, Hands, HandList), 
  nth1(Suit, HandList, SuitList), 
  maplist(suit_quality_score, SuitList, Scores), 
  sum_list(Scores, QualityScore). 

suit_quality_score(a, 6).
suit_quality_score(k, 5).
suit_quality_score(q, 4).
suit_quality_score(j, 3).
suit_quality_score(t, 2).
suit_quality_score(9, 1).
suit_quality_score(_, 0).

% Imp scores

get_imp_score(ID1, ID2, Hand, Imps) :-
  ( Hand == 1 ; Hand == 3), 
  deal_feature(ID1, score(Score1)), 
  deal_feature(ID2, score(Score2)), 
  Score is Score1 - Score2, 
  score_imps(Score, Imps). 
get_imp_score(ID1, ID2, Hand, Imps) :-
  ( Hand == 2 ; Hand == 4), 
  get_imp_score(ID1, ID2, 1, OppImps), 
  Imps is -1 * OppImps. 

score_imps(Score, Imps) :-
  AbsScore is abs(Score),
  imp_table(AbsScore, AbsImps), 
  ( Score >= 0
  , Imps is AbsImps
  ; Score < 0
  , Imps is -1 * AbsImps
  ). 

imp_table(Score, Imps) :-
  ( Score =< 10, Imps is 0
  ; Score >= 20, Score =< 40, Imps is 1
  ; Score >= 50, Score =< 80, Imps is 2
  ; Score >= 90, Score =< 120, Imps is 3
  ; Score >= 130, Score =< 160, Imps is 4
  ; Score >= 170, Score =< 210, Imps is 5
  ; Score >= 220, Score =< 260, Imps is 6
  ; Score >= 270, Score =< 310, Imps is 7
  ; Score >= 320, Score =< 360, Imps is 8
  ; Score >= 370, Score =< 420, Imps is 9
  ; Score >= 430, Score =< 490, Imps is 10
  ; Score >= 500, Score =< 590, Imps is 11
  ; Score >= 600, Score =< 740, Imps is 12
  ; Score >= 750, Score =< 890, Imps is 13
  ; Score >= 900, Score =< 1090, Imps is 14
  ; Score >= 1100, Score =< 1290, Imps is 15
  ; Score >= 1300, Score =< 1490, Imps is 16
  ; Score >= 1500, Score =< 1740, Imps is 17
  ; Score >= 1750, Score =< 1990, Imps is 18
  ; Score >= 2000, Score =< 2240, Imps is 19
  ; Score >= 2250, Imps is 20
  ). 


