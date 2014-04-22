/*  File        statistics.pl
 *  Part of     bidstat: analysis of bridge bidding 
 *  Author      Guus Schreiber, guus.schreiber@vu.nl
 *  Works with  SWI-Prolog 7.1.3
 */


/*------------------------------------------------------------
 *  Queries for building statistical data tables
 *------------------------------------------------------------*/

% stat/1
% 
% Generic predicate for checking one feature for all deals
% Each feature is defined using a statistics/3 predicate

stat(StatFeature) :-
  atom_concat(StatFeature, '.txt', FileName), 
  open(FileName, write, Stream, [alias(output)]),
  format(output, "ID Year ~w~n", [StatFeature]),   
  forall(deal(ID, _), 
    ( ( error_auction(ID)      % error in the PBN source, skip
      ; empty_auction(ID)
      ; year(ID, Year)
      , statistics(StatFeature, ID, Values)
      , format(output, "~w ~w", [ID, Year])
      , forall(member(Value, Values), 
               format(output," ~w",[Value]))
      , format(output, "~n", [])
      ))),
  close(Stream). 

empty_auction(ID) :-
  deal(ID, [_, _, _, _, Auction, Contract, _, _]),
  ( Auction == []
  ; sum_list(Auction, N) 
  , N == 0
  , Contract \= pass).

% Frequency first hand is opened, independnt of hand type 
% 1=true, 0=false

statistics(opening_first_hand, ID, [Bool]) :-
  bid(ID, 1, 1, _, Features), !,
  ( memberchk(opening, Features)
  , Bool is 1
  ; Bool is 0
  ).  
statistics(opening_first_hand, _, ['NA']).


% Frequency of contested auction

statistics(contested_auction, ID, [Bool]) :-
  findall(Bid, bid(ID, _, _, Bid, _), Bids), 
  split_bids(Bids, DealerSide, OppSide),
  sum_list(DealerSide, Num1),
  sum_list(OppSide, Num2),
  ( Num1 == 0, Num2 == 0, Bool = "NA" 
  ; Num1 > 0, Num2 > 0, Bool is 1     
  ; Bool is 0
  ). 


% Frequency balanced (4333/4432/5332) in 1st psotion
% 1=BAL, 0=other

statistics(balanced_first_hand, ID, [1]) :-
  hand(ID, 1, _, _, Features),
  memberchk(balanced, Features). 
statistics(balanced_first_hand, _, [0]).


% Frequency opening BAL 11-12 HCP in 1st hand

statistics(opening_11_12_bal_1, ID, [Bool]) :-
  hand(ID, 1, HCP, _, HandFeatures),  
  ( HCP == 11 ; HCP == 12),
  memberchk(balanced, HandFeatures),
  ( bid(ID, 1, 1, _, Features)
  , memberchk(opening, Features)
  , Bool is 1
  ; Bool is 0 ).
statistics(opening_11_12_bal_1, _, ['NA']).

% Frequency opening BAL 11-12 HCP in 1st/2nd hand

statistics(opening_11_12_bal_2, ID, [Bool]) :-
  hand(ID, 1, HCP, _, HandFeatures),  
  ( HCP == 11 ; HCP == 12),
  memberchk(balanced, HandFeatures),
  ( bid(ID, 1, 1, _, Features)
  , memberchk(opening, Features)
  , Bool is 1
  ; Bool is 0 ).
statistics(opening_11_12_bal_2, ID, [Bool]) :-
  bid(ID, 1, 1, 0, _), 
  hand(ID, 2, HCP, _, HandFeatures),  
  ( HCP == 11 ; HCP == 12),
  memberchk(balanced, HandFeatures),
  ( bid(ID, 2, 1, _, Features)
  , memberchk(opening, Features)
  , Bool is 1
  ; Bool is 0 ). 
statistics(opening_11_12_bal_2, _, ['NA']).


% Frequency of preempts

statistics(preempt, ID, [Diff]) :-
  is_preempt(ID, 1, Level, Length),
  Diff is Length - Level, 
  ( Diff > 1 
  ; format("Not included: Deal ~w, diff is ~w~n", [ID, Diff])
  , fail
  ). 
statistics(preempt, _, [0]). 

is_preempt(_, 5, _, _) :- !, fail.  
is_preempt(ID, Hand, Level, Length) :-
  hand(ID, Hand, HCP, [_, [Length|_]], _),
  HCP < 10, 
  bid(ID, Hand, 1, Bid, Features),
  Bid mod 10 \= 5,
  bid_class(preempt, Features),
  Level is Bid div 10.
is_preempt(ID, Hand, Level, Length) :-
  NextHand is Hand + 1,
  is_preempt(ID, NextHand, Level, Length). 

bid_class(preempt, Features) :-
  memberchk(jump_opening, Features). 
bid_class(preempt, Features) :-
  bid_class(jump, Features), 
  bid_class(overcall, Features). 

bid_class(jump, Features) :-
  memberchk(jump_single, Features). 
bid_class(jump, Features) :-
  memberchk(jump_double, Features). 
bid_class(jump, Features) :-
  memberchk(jump_triple_plus, Features). 

bid_class(overcall, Features) :-
  memberchk(overcall_direct, Features). 
bid_class(overcall, Features) :-
  memberchk(overcall_live, Features). 
     

% Frequency of preempt 1st hand any/NV

statistics(weak_two_1st, ID, [Value]) :-
  hand(ID, 1, HCP, [_, [6|_]], _), 
  HCP < 10, 
  bid(ID, 1, 1, Bid, _), 
  Value is Bid div 10. 
statistics(weak_two_1st, _, ["NA"]).

statistics(weak_two_1st_nv, ID, [Value]) :-
  deal(ID, [_, _, Vul, _, _, _, _, _]),
  ( Vul == 1 ; Vul == 3), 
  hand(ID, 1, HCP, [_, [6|_]], _), 
  HCP < 10, 
  bid(ID, 1, 1, Bid, _), 
  Value is Bid div 10. 
statistics(weak_two_1st_nv, _, ["NA"]).
    

% Auxilliary predicates for statistics

year(ID, Year) :-
  (  deal(ID, [Year|_])
  ; Year = 'NA'
  ).  

split_bids([B1,B2,B3,B4,B5,B6,B7,B8],[B1,B3,B5,B7],[B2,B4,B6,B8]).
split_bids([B1,B2,B3,B4,B5,B6,B7],[B1,B3,B5,B7],[B2,B4,B6]).
split_bids([B1,B2,B3,B4,B5,B6],[B1,B3,B5],[B2,B4,B6]).
split_bids([B1,B2,B3,B4,B5],[B1,B3,B5],[B2,B4]).
split_bids([B1,B2,B3,B4],[B1,B3],[B2,B4]).
split_bids([B1,B2,B3],[B1,B3],[B2]).
split_bids([B1,B2],[B1],[B2]).
split_bids([B1],[B1],[]).
split_bids([],[],[]).   
