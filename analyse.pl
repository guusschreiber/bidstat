:- dynamic(deal/2). 

testdeal(deal1,
  [1955,
  [
   [[a,k,7,5],[5,4],[9,8,7,6,5,3],[a]],
   [[q],[a,t,9,8,3],[4,2],[j,t,7,5,3]],
   [[6,4,2],[k,q,j,7],[a,q,j],[9,6,2]],
   [[j,t,9,8,3],[6,2],[k,t],[k,q,8,4]]
  ],
  1,
  imp,
  [12, 13, 7, 0, 0, 31, 62, 7, 0, 0, 0],
  [3,d],
  1,
  9]). 

%% STATISTICS

stat(StatFeature) :-
  concat(StatFeature, '.txt', FileName), 
  open(FileName, write, Stream),
  tell(Stream), 
  writef("ID Year %w\n", [StatFeature]),  
  findall(ID, deal(ID, _), IDs), 
  forall(member(ID, IDs), 
    ( year(ID, Year)
    , statistics(StatFeature, ID, Value)
    , writef("%w %w %w\n", [ID, Year, Value]) 
    )),
    told. 

% Frequency first hand is opened, independnt of hand type 
% 1=true, 0=false
%
statistics(opening_first_hand, ID, Bool) :-
  bid(ID, 1, 1, _, Features), !,
  ( memberchk(suit_or_nt, Features)
  , Bool is 1
  ; Bool is 0
  ).  
statistics(opening_first_hand, _, 'NA').
%
% Frequency balanced (4333/4432/5332) in 1st psotion
% 1=BAL, 2=other
%
statistics(balanced_first_hand, ID, 1) :-
  hand(ID, 1, _, _, Features),
  memberchk(balanced, Features). 
statistics(balanced_first_hand, _, 0).
%
% Frequency opening 11-12 HCP in 1st/2nd hand, regardless distr
%
statistics(opening_11_12_hcp, ID, 1) :-
  hand(ID, 1, HCP, _, _),  
  ( HCP == 11 ; HCP == 12),
  bid(ID, 1, 1, _, [Features]),
  memberchk(opening, Features).
statistics(opening_11_12_hcp, ID, 1) :-
  bid(ID, 2, HCP, _, _),  
  ( HCP == 11 ; HCP == 12),
  hand(ID, 2, 1, _, [Features]),
  memberchk(opening, Features).
statistics(opening_11_12_hcp, _, 0).


% Auxilliary predicates for statistics

year(ID, Year) :-
  (  deal(ID, [Year|_])
  ; Year = 'NA'
  ).  
   

   

%% HAND AND BID TYPE DEFINTIONS

defs :-  forall(deal(ID, _), deal_def(ID)). 

deal_def(ID) :-
  maplist(hand_def(ID), [1,2, 3, 4]),
  bids_def(ID, 1, 9). 

% Hand type defintions

hand_def(ID, H) :-
  hands(ID, Hands), 
  nth1(H, Hands, Hand), 
  hcp(Hand, HCP),
  distr(Hand, Distr), 
  findall(Type, hand_type(Distr, Type), HandTypes), 
  HandDef =.. [hand, ID, H, HCP, Distr, HandTypes],
  write_ln(HandDef), 
  assert(HandDef). 

hands(ID, Hands) :-
  deal(ID, Facts),
  nth1(2, Facts, Hands). 

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

% add here more handtypes: hand_type(+Distr, ?HandType)

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
suittype([_, [S1, S2, _, _]], single-suited) :-
  S1 >= 5, S2 =< 3. 
suittype([_, [S1, 4, _, _]], two_suited_short) :- 
  S1 >= 5.
suittype([_, [_, S2, _, _]], two_suited_long) :-
  S2 >= 5. 
suittype([_, [_, _, 4, _]], three_suited). 

% Bid type definitions (two types)

bids_def(_, N, N). 
bids_def(ID, N, _) :-
  deal(ID, Facts),
  nth1(5, Facts, Auction),
  length(Auction, L), 
  N > L. 
bids_def(ID, N, Max) :-
  deal(ID, Facts),
  nth1(5, Facts, Auction),
  nth1(N, Auction, Bid),
  list_before(N, Auction, BidHistory),  
  bid_def(Bid, BidHistory, Features),
  ( N > 4
  , Player is N - 4
  , Round is 2
  ; Player is N
  , Round is 1
  ), 
  BidDef =.. [bid, ID, Player, Round, Bid, Features], 
  assert(BidDef),
  write_ln(BidDef), 
  NextN is N + 1, 
  bids_def(ID, NextN, Max). 

list_before(1, _, []).
list_before(N, [E|Rest1], [E|Rest2]) :-
  NewN is N - 1, 
  list_before(NewN, Rest1, Rest2).

bid_def(Bid, BidHistory, Features) :-
  findall(F, call_type(F, Bid), CallTypes), 
  findall(F, bid_feature(F, Bid, BidHistory), BidFeatures),
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

bid_feature(opening, Bid, BidHistory) :-
  call_type(suit_or_nt, Bid),
  \+ bid_history(BidHistory, _, action, _).
bid_feature(overcall, Bid, BidHistory) :-
  call_type(suit_or_nt, Bid),
  bid_history(BidHistory, 1, suit_or_nt, _). 
bid_feature(SomeJump, Bid, BidHistory) :-
  bid_history(BidHistory, _, suit_or_nt, PreviousBid),
  bid_diff(Bid, PreviousBid, Diff),
  ( Diff > 10, Diff =< 20, SomeJump = jump_single
  ; Diff > 20, Diff =< 30, SomeJump = jump_double
  ; Diff > 30, SomeJump = jump_triple_plus
  ), !. 

bid_history(History, Depth, Type, Bid) :-
  var(Depth),
  reverse(History, Previous),
  find_call(Previous, 1, Depth, Type, Bid).
bid_history(History, Depth, Type, Bid) :-
  \+ var(Depth),
  reverse(History, Previous),
  nth1(Depth, Previous, Bid),
  call_type(Type, Bid).    
 
find_call([Bid|_], Pos, Pos, Type, Bid) :-
  call_type(Type, Bid). 
find_call([_|Rest], CurrentDepth, Depth, Type, Bid) :-
  NewDepth is CurrentDepth +1,
  find_call(Rest, NewDepth, Depth, Type, Bid).   

bid_diff(Bid, PreviousBid, Diff) :-
  Diff is Bid - PreviousBid. 
 

% INIT
%
% This code reads the output of pbn2prolog and transforms the
% relevant data in computation-friendly format 
% (typically atoms to numbers);
%
% * NS/EW information is not preserved (not needed).
%   Dealer is simply player 1, etc.
% * Vulnerablility: 1 = none, 2 = dealer-side V, etc
% * Result is 0 if passed out; score info not preserved
% * Bid representation: 0=pass, 7/8=dbl/rdbl, 11=1C, ect


init :-
  protocol('initlog.txt'),
  retractall(deal/2),
  reset_gensym(deal_),
  open('db.pl', read, Stream), 
  see(Stream), 
  read(Deal), 
  process_deal(Deal), 
  close(Stream),
  noprotocol.

process_deal(end_of_file). 
process_deal(deal([end_of_deal])) :-
  read(Deal), 
  process_deal(Deal).
process_deal(deal(Args)) :-  
  process_args(Args, Facts),
  gensym(deal_, ID),
  assert(deal(ID, Facts)),
  write_ln(deal(ID, Facts)), 
  read(Deal),
  process_deal(Deal).
process_deal(_) :-
  read(Deal), 
  process_deal(Deal). 

process_args(Args, [Y, H, V, S, A, C, D, R]) :-
  process_dealer(Args, Dealer), 
  process_year(Args, Y), 
  process_hand(Args, Dealer, H),
  process_vul(Args, Dealer, V), 
  process_scoring(Args, S), 
  process_auction(Args, Dealer, A),
  process_contract(Args, C), 
  process_declarer(Args, Dealer, C, D), 
  process_result(Args, C, R).  

process_dealer(Args, Dealer) :-
  ( memberchk(tag('Dealer', Dealer), Args)
  , nsew(Dealer)
  ; write_ln("Error: no Dealer")
  , fail
  ), !.  

nsew(n).
nsew(e).
nsew(s).
nsew(w).

process_year(Args, Year) :-
  ( memberchk(tag('Date', [Year|_]), Args)
  ; write_ln("Error: no Year")
  , fail
  ), !.

process_hand(Args, Dealer, H) :-
  ( memberchk(tag('Deal', Hand), Args)
  , \+ var(Hand)
  , process_h(Dealer, Hand, H)
  ; writef("Error: no Hand: \n%t", [Args])
  , fail
  ), !.
 
process_h(Dealer, [Dealer | Hand ], Hand). 
process_h(Dealer, [First | HandList], Hand) :-
  distance(First, Dealer, Distance),
  transpose(Distance, HandList, Hand),
  writef("Transposed: %w %w\n", [HandList, Hand]). 

distance(n, e, 1).
distance(n, s, 2).
distance(n, w, 3).
distance(e, s, 1).
distance(e, w, 2).
distance(e, n, 3).
distance(s, w, 1).
distance(s, n, 2).
distance(s, e, 3).
distance(w, n, 1).
distance(w, e, 2).
distance(w, s, 3).

transpose(1, [H1, H2, H3, H4], [H2, H3, H4, H1]). 
transpose(2, [H1, H2, H3, H4], [H3, H4, H1, H2]). 
transpose(3, [H1, H2, H3, H4], [H4, H1, H2, H3]).

process_vul(Args, Dealer, Vul) :-
  memberchk(tag('Vulnerable', V), Args),
  ( \+ var(V)
  , process_v(Dealer, V, Vul)
  ; write_ln("Error: no Vulnerability")
  , fail
  ), !. 

process_v(_, 'None', 1). 
process_v(_, 'Love', 1). 
process_v(Dealer, 'NS', 2) :-
   ( Dealer == n ; Dealer == s ). 
process_v(Dealer, 'EW', 2) :-
   ( Dealer == e ; Dealer == w ). 
process_v(Dealer, 'NS', 3) :-
   ( Dealer == e ; Dealer == w ). 
process_v(Dealer, 'EW', 3) :-
   ( Dealer == n ; Dealer == s ). 
process_v(_, 'All', 4). 
process_v(_, 'Both', 4). 

process_scoring(Args, Scoring) :-
  memberchk(tag('Scoring', S), Args),
  process_s(S, Scoring), !.

process_s('IMP', 1).
process_s('IMP_1948', 1).
process_s('IMP_1961', 1).
process_s(_, 0).

process_auction(Args, _, _) :-
  \+ memberchk(tag('Auction', _), Args),
  writef("Error:  no Auction: \n%t\n", [Args]), 
  fail, !. 
process_auction(Args, Dealer, Auction) :-
  memberchk(tag('Auction', Hand), Args), 
  ( Dealer == Hand
  ; writef("Warning: dealer/auction mismatch: \n%t\n", [Args])
  ), 
  find_bids(Args, BidList), 
  maplist(bid_to_num, BidList, Auction). 

find_bids([br(Bids)|RestArgs], Auction) :-
  find_bids(RestArgs, RestBids), 
  append(Bids, RestBids, Auction).
find_bids([_|RestArgs], Auction) :-
  find_bids(RestArgs, Auction). 
find_bids([], []). 

bid_to_num(pass, 0). 
bid_to_num(ap, 0). 
bid_to_num(x, 7).
bid_to_num(xx, 8). 
bid_to_num([Level, Color], BidNum) :-
  color_to_num(Color, ColorNum),
  BidNum is 10 * Level + ColorNum. 

color_to_num(c, 1). 
color_to_num(d, 2). 
color_to_num(h, 3). 
color_to_num(s, 4). 
color_to_num(n, 5). 

process_contract(Args, C) :-
  memberchk(tag('Contract', C), Args),
  \+ var(C), !.  

process_declarer(Args, Dealer, Contract, Declarer) :-
  memberchk(tag('Declarer', DeclarerNSEW), Args),
  process_d(Dealer, Contract, DeclarerNSEW, Declarer), !. 

process_d(_, pass, _, 0).
process_d(n, _, n, 1). 
process_d(n, _, e, 2). 
process_d(n, _, s, 3). 
process_d(n, _, w, 4).
process_d(e, _, e, 1). 
process_d(e, _, s, 2). 
process_d(e, _, w, 3). 
process_d(e, _, n, 4).
process_d(s, _, s, 1). 
process_d(s, _, w, 2). 
process_d(s, _, n, 3). 
process_d(s, _, e, 4).
process_d(w, _, w, 1). 
process_d(w, _, n, 2). 
process_d(w, _, e, 3). 
process_d(w, _, s, 4). 

process_result(Args, Contract, R) :-
  memberchk(tag('Result', Result), Args),
  process_r(Result, Contract, R), !. 

process_r(_, pass, 0). 
process_r(R, _, R). 
