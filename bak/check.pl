/*  File        check.pl
 *  Part of     bidstat: analysis of bridge bidding 
 *  Author      Guus Schreiber, guus.schreiber@vu.nl
 *  Works with  SWI-Prolog 7.1.3
 */


/*------------------------------------------------------------
 *  Checking hand records for errors
 *------------------------------------------------------------*/

:- discontiguous(check/3). 

check_dataset(Dataset) :-
  atom_concat(Dataset, '_errors.pl', ErrorFile),
  open(ErrorFile, write, ErrorStream, [alias(error)]), 
  findall(ID, deal(ID, _), IDs), 
  maplist(check_deal, IDs, Errors),
  sum_list(Errors, NumErrors), 
  format(user, "Number of erroneous deals found: ~a~n", [NumErrors]), 
  check_equal_deals,
  close(ErrorStream). 

check_deal(ID, DealError) :-
  findall(Test, check_feature(Test), Tests), 
  maplist(check(ID), Tests, ErrorLists), 
  flatten(ErrorLists, Errors), 
  ( Errors == []
  , DealError is 0
  ; write_term(error, error(ID, Errors), [fullstop(true), nl(true)])
  , DealError is 1
  ). 
  
check_feature(missing_features).
check_feature(hands).
check_feature(contract).
check_feature(declarer). 
check_feature(fake_auction).

% Core features mossing from the raw data will lead to an error
% This makes use of the list of features to be included, see process.pl

check(ID, missing_features, Errors) :-
  findall(Feature, include_feature(Feature), Features),
  maplist(missing_feature(ID), Features, Errors).

missing_feature(ID, Feature, Error) :-
  ( Fact =.. [Feature, _] 
  , deal(ID, Facts)
  , member(Fact, Facts) 
  , Error = []
  ; Error = issue(Feature, missing)
  ). 

% Check that there rae indeed 4 hands with 13 cards, 
% Should maybe also include a complete 52-card check

check(ID, hands, issue(hands, Hands)) :-
  deal_feature(ID, hands(Hands)),
  \+ correct_hands(Hands).

correct_hands(Hands) :-
  length(Hands, 4), 
  maplist(correct_hand, Hands). 

correct_hand(Hand) :-
  flatten(Hand, Cards), 
  length(Cards, 13). 

% Check whether contract matches auction

check(ID, contract, issue(contract, incorrect(C, Auction))) :-
  deal_feature(ID, auction(Auction)), 
  deal_feature(ID, contract(C)),
  contract_num(C, Contract), 
  \+ get_contract(Auction, Contract, _), !. 

contract_num(pass, 0). 
contract_num([Level, Suit, _], Contract) :-
  contract_num([Level, Suit], Contract). 
contract_num([Level, Suit], Contract) :-
  suit_num(Suit, SuitNum), 
  Contract is (Level * 10) + SuitNum. 

suit_num(c, 1).
suit_num(d, 2).
suit_num(h, 3).
suit_num(s, 4).
suit_num(n, 5).

get_contract(Auction, Contract, Position) :-
  reverse(Auction, RevAuction),
  last_bid(RevAuction, Contract, Position), !.

last_bid([Bid | Rest], Bid, Position) :-
  Bid > 10, 
  length(Rest, L), 
  Position is L + 1. 
last_bid([_ | Rest], Bid, Position) :-
  last_bid(Rest, Bid, Position). 
last_bid(_, 0, 0). 

% Check whether declarer is correct

check(ID, declarer, []) :-
  deal_feature(ID, contract(C)),
  deal_feature(ID, declarer(Declarer)), 
  C == pass,
  Declarer == 0.
check(ID, declarer, issue(declarer, incorrect(Declarer, C, Auction))) :-
  deal_feature(ID, auction(Auction)), 
  deal_feature(ID, contract(C)),
  deal_feature(ID, declarer(Declarer)), 
  once(get_contract(Auction, Contract, Position)),
  Suit is Contract mod 10,
  once(auction_declarer(Auction, Suit, Position, AuctionDeclarer)),
  AuctionDeclarer \= Declarer. 

auction_declarer(Auction, Suit, Position, Declarer) :-
  declarer_side_bids(Position, BidNums), 
  get_declaring_bid(BidNums, Auction, Suit, BidNum), !, 
  Tmp is BidNum mod 4,
  ( Tmp == 0
  , Declarer is 4
  ; Declarer is Tmp
  ).

declarer_side_bids(Position, BidNums) :-
  Position > 0,
  Side is Position mod 2,
  ( Side == 0
  , First = 2
  ; First = 1
  ),
  get_odd_or_even_bids(First, Position, BidNums).

get_odd_or_even_bids(Position, Position, [Position]). 
get_odd_or_even_bids(Current, Position, [Current | Rest]) :-
  Next is Current + 2,  
  get_odd_or_even_bids(Next, Position, Rest).

get_declaring_bid([BidNum | _], Auction, Suit, BidNum) :-
  nth1(BidNum, Auction, Bid), 
  BidType is Bid mod 10, 
  BidType == Suit.
get_declaring_bid([_ | Rest], Auction, Suit, BidNum) :-
  get_declaring_bid(Rest, Auction, Suit, BidNum).

% Check for bidding shortcuts (auction probably lost)

check(ID, fake_auction, issue(auction, fake(Auction))) :-
  deal_feature(ID, auction(Auction)),
  single_bid(Auction, Bid, 1, Hand),
  Level is Bid div 10,
  hand(ID, Hand, HCP, [_, [LongestSuit |_]], _), 
  Level > 1,
  ( BidType is Bid mod 10
  , BidType == 5
  , HCP < 18
  ; Diff is LongestSuit - Level
  , Diff < 2
  ). 

single_bid(_, _, 5, _) :- !, fail. 
single_bid([Bid | Rest], Bid, Hand, Hand) :-
  Bid > 10, 
  forall(member(B, Rest), B < 10).   
single_bid([B | Rest], Bid, Prev, Hand) :-
  B < 10, 
  Current is Prev + 1,
  single_bid(Rest, Bid, Current, Hand). 

% Catchall for ID check

check(_, _, []). 

% Check for equal deals in different datasets

check_equal_deals :-
  forall(equal_deal(ID1, ID2), check_equal_deal(ID1, ID2)).

check_equal_deal(ID1, ID2) :- 
  from_dataset(ID1, D1), 
  from_dataset(ID2, D2),
  ( D1 == D2 
  ; format(user, "Warning: duplicate deals ~a ~a~n", [ID1, ID2])
  ).  

from_dataset(ID, Dataset) :-
  sub_atom(ID, 2, 1, _, '_'), 
  sub_atom(ID, 0, 2, _, Dataset).
from_dataset(ID, Dataset) :-
  sub_atom(ID, 3, 1, _, '_'), 
  sub_atom(ID, 0, 3, _, Dataset).
