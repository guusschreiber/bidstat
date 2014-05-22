/*  File        definitions.pl
 *  Part of     bidstat: analysis of bridge bidding 
 *  Author      Guus Schreiber, guus.schreiber@vu.nl
 *  Works with  SWI-Prolog 7.1.3
 */


/*------------------------------------------------------------
 *  Definitions of hand and bid types
 *------------------------------------------------------------*/


deal_record_def(ID) :-
  hand_types(ID), 
  bid_types(ID, 8). 

% Hand type defintions
% This uses the predicates "correct_hand predicates from check.pl

hand_types(ID) :-
  ( \+ record_feature(ID, hands(_))
  ; record_feature(ID, hand(Hands))
  , \+ correct_hands(Hands)
  ; record_feature(ID, hands(Hands))
  , maplist(hand_def(ID, Hands), [1,2,3,4])
  ). 

hand_def(ID, Hands, H) :-
  nth1(H, Hands, Hand), 
  hcp(Hand, HCP),
  distr(Hand, Distr), 
  findall(Type, hand_type(Distr, Type), HandTypes), 
  HandDef =.. [hand, ID, H, HCP, Distr, HandTypes],
  assert(HandDef), 
  write_term(data, HandDef, [fullstop(true), nl(true)]).

hcp(Hand, HCP) :-
  flatten(Hand, H), 
  maplist(card_hcp, H, HCPList),
  sum_list(HCPList, HCP). 

card_hcp(a, 4).
card_hcp(k, 3).
card_hcp(q, 2).
card_hcp(j, 1).
card_hcp(t, 0). 
card_hcp(9, 0). 
card_hcp(8, 0). 
card_hcp(7, 0). 
card_hcp(6, 0). 
card_hcp(5, 0). 
card_hcp(4, 0). 
card_hcp(3, 0). 
card_hcp(2, 0). 

distr(Hand, [Actual, Ordered]) :-
  maplist(length, Hand, Actual),
  msort(Actual, Tmp),
  reverse(Tmp, Ordered). 

hand_type([[_, 5, _, _], _], suit_5M).
hand_type([[5, _, _, _], _], suit_5M).

hand_type(Distr, BalType) :-
  baltype(Distr, BalType). 
hand_type(Distr, SuitType) :-
  suittype(Distr, SuitType).

baltype([_, [4, 3, 3, 3]], balanced).
baltype([_, [4, 4, 3, 2]], balanced).
baltype([_, [5, 3, 3, 2]], balanced). 
baltype([_, [5, 4, 2, 2]], semi_balanced). 
baltype([_, [6, 3, 2, 2]], semi_balanced). 
baltype([_, [7, 2, 2, 2]], semi_balanced). 
baltype([_, [_, _, _, 1]], unbalanced). 
baltype([_, [_, _, _, 0]], unbalanced). 

suittype([_, [4, _, _, _]], balanced_suited). 
suittype([_, [S1, S2, _, _]], single_suited) :-
  S1 >= 5, S2 =< 3. 
suittype([_, [S1, 4, _, _]], two_suited_short) :- 
  S1 >= 5.
suittype([_, [_, S2, _, _]], two_suited_long) :-
  S2 >= 5. 
suittype([_, [_, _, 4, _]], three_suited). 

% Bid type definitions (two types)

bid_types(ID, _) :-
  \+ record_feature(ID, auction(_)).
bid_types(ID, N) :-
  record_feature(ID, auction(Auction)),
  length(Auction, L), 
  ( L < N
  , Num is L
  ; Num is N
  ), 
  bids_def(ID, Auction, 0, Num). 
 
bids_def(_, _, N, N). 
bids_def(ID, Auction, N, Max) :-
  BidNum is N + 1,  
  nth1(BidNum, Auction, Bid),
  player_position(BidNum, Player, Round), 
  list_before(BidNum, Auction, BidHistory), 
  bid_def(ID, Player, Round, Bid, BidHistory, Features),
  BidDef =.. [bid, ID, Player, Round, Bid, Features], 
  assert(BidDef), 
  write_term(data, BidDef, [fullstop(true), nl(true)]),
  bids_def(ID, Auction, BidNum, Max). 

bid_def(ID, Player, Round, Bid, BidHistory, Features) :-
  findall(F, call_type(F, Bid), CallTypes), 
  findall(F, 
    bid_feature(F, ID, Player, Round, Bid, BidHistory), 
    BidFeatures),
  append(CallTypes, BidFeatures, Features).  

% Bid features, indepedent of previous bids

call_type(clubs, Bid) :-
  Bid mod 10 =:= 1. 
call_type(diamonds, Bid) :-
  Bid mod 10 =:= 2. 
call_type(hearts, Bid) :-
  Bid mod 10 =:= 3. 
call_type(spades, Bid) :-
  Bid mod 10 =:= 4. 
call_type(minor, Bid) :-
  ( call_type(clubs, Bid)
  ; call_type(diamonds, Bid)
  ).
call_type(major, Bid) :-
  ( call_type(hearts, Bid)
  ; call_type(spades, Bid)
  ).
call_type(suit, Bid) :-
  N is Bid mod 10,
  N > 0, N < 5.
call_type(nt, Bid) :-
  Bid mod 10 =:= 5. 
call_type(suit_or_nt, Bid) :-
  Bid > 10.
call_type(pass, 0). 
call_type(double, 7).
call_type(redouble, 8).
call_type(action, Bid) :-
  Bid > 0. 
call_type(level_1, Bid) :-
  call_type(suit_or_nt, Bid), 
  Bid div 10 =:= 1.  
call_type(level_2, Bid) :-
  call_type(suit_or_nt, Bid), 
  Bid div 10 =:= 2.  
call_type(level_3, Bid) :-
  call_type(suit_or_nt, Bid), 
  Bid div 10 =:= 3.  
call_type(level_4plus, Bid) :-
  call_type(suit_or_nt, Bid), 
  Bid div 10 > 3.   

% Bid features, dependent on previous bids

bid_feature(opening, _, _, 1, Bid, BidHistory) :-
  call_type(suit_or_nt, Bid),
  \+ bid_history(BidHistory, _, action, _).
bid_feature(response_opening, ID, _, _, Bid, BidHistory) :-
  call_type(action, Bid),
  previous_bid(partner, BidHistory, Partner, Round, HisBid, History), 
  bid_feature(opening, ID, Partner, Round, HisBid, History).
bid_feature(overcall_direct, ID, _, _, Bid, BidHistory) :-
  call_type(suit_or_nt, Bid),
  previous_bid(rho, BidHistory, RHO, Round, HisBid, History),
  bid_feature(opening, ID, RHO, Round, HisBid, History). 
bid_feature(overcall_live, ID, _, _, Bid, BidHistory) :-
  call_type(suit_or_nt, Bid),
  previous_bid(lho, BidHistory, LHO, R1, B1, H1),
  bid_feature(opening, ID, LHO, R1, B1, H1), 
  previous_bid(partner, BidHistory, _, _, B2, _),
  call_type(pass, B2), 
  previous_bid(rho, BidHistory, _, _, B3, _),
  call_type(suit_or_nt, B3).
bid_feature(overcall_4th_hand, ID, _, _, Bid, BidHistory) :-
  call_type(suit_or_nt, Bid),
  previous_bid(lho, BidHistory, LHO, R1, B1, H1),
  bid_feature(opening, ID, LHO, R1, B1, H1), 
  previous_bid(partner, BidHistory, _, _, B2, _),
  call_type(pass, B2), 
  previous_bid(rho, BidHistory, _, _, B3, _),
  call_type(pass, B3).
bid_feature(SomeJump, _, _, _, Bid, BidHistory) :-
  bid_history(BidHistory, _, suit_or_nt, PreviousBid),
  Diff is Bid - PreviousBid,
  ( Diff > 10, Diff =< 20, SomeJump = jump_single
  ; Diff > 20, Diff =< 30, SomeJump = jump_double
  ; Diff > 30, SomeJump = jump_triple_plus
  ), !. 
bid_feature(jump_opening, ID, Player, Round, Bid, BidHistory) :-
  Bid > 20,
  bid_feature(opening, ID, Player, Round, Bid, BidHistory). 

previous_bid(rho, BidHistory, Player, Round, Bid, History) :-
  pre_bid(0, BidHistory, Player, Round, Bid, History).
previous_bid(partner, BidHistory, Player, Round, Bid, History) :-
  pre_bid(1, BidHistory, Player, Round, Bid, History).
previous_bid(lho, BidHistory, Player, Round, Bid, History) :-
  pre_bid(2, BidHistory, Player, Round, Bid, History).

pre_bid(Depth, BidHistory, Player, Round, Bid, History) :-
  length(BidHistory, Num),
  Pos is Num - Depth,  
  player_position(Pos, Player, Round), 
  nth1(Pos, BidHistory, Bid),
  list_before(Pos, BidHistory, History). 

player_position(1, 1, 1).
player_position(2, 2, 1).
player_position(3, 3, 1).
player_position(4, 4, 1).
player_position(5, 1, 2).
player_position(6, 2, 2).
player_position(7, 3, 2).
player_position(8, 4, 2).

list_before(1, _, []).
list_before(N, [E|Rest1], [E|Rest2]) :-
  NewN is N - 1, 
  list_before(NewN, Rest1, Rest2).

bid_history(History, Depth, Type, Bid) :-
  var(Depth),
  reverse(History, H),
  find_call(H, 1, Depth, Type, Bid), !.
bid_history(History, Depth, Type, Bid) :-
  \+ var(Depth),
  reverse(History, H), 
  nth1(Depth, H, Bid),
  call_type(Type, Bid).    
 
find_call([Bid|_], Pos, Pos, Type, Bid) :-
  call_type(Type, Bid). 
find_call([_|Rest], CurrentDepth, Depth, Type, Bid) :-
  NewDepth is CurrentDepth +1,
  find_call(Rest, NewDepth, Depth, Type, Bid).   

bid_diff(Bid, PreviousBid, Diff) :-
  Diff is Bid - PreviousBid. 
