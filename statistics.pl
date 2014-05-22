/*  File        statistics.pl
 *  Part of     bidstat: analysis of bridge bidding 
 *  Author      Guus Schreiber, guus.schreiber@vu.nl
 *  Works with  SWI-Prolog 7.1.3
 */

/*--------------------------------------------------
 *  Queries for building statistical data tables
 *---------------------------------------------------*/

% stat/2
% Generic predicate for generating a data table
% Two types: 
% 1. Checking all deals for a particular pattern
%    Patterns are defined using deal_profile/3
% 2. Checking set of identical deals (minimum 2)
%    with the purpose of comparing results for 
%    different bids with hands with the same profile
%    Defined using set_profile/2
% 
% Both will ignore all deals identified as erroneous 
% in <dataset>_errors.pl.

stat(Dataset, StatFeature) :-
  labels(StatFeature, Labels),
  init_stat(Dataset, StatFeature, Labels, Stream), 
  stat_all_records(StatFeature), 
  format(user,"Data table generated: ~a~n",[StatFeature]),
  close(Stream). 
stat(Dataset, StatFeature) :-
  deal_profile(StatFeature, Profile), 
  memberchk(labels(Labels), Profile), 
  init_stat(Dataset, StatFeature, Labels, Stream),   
  assert(required_info(result)),
  stat_all_deals(StatFeature),
  format(user,"Data table generated: ~a~n",[StatFeature]),
  close(Stream). 

stat_all_records(StatFeature) :-
  forall(deal_record(ID, _), 
    ( ( \+ suitable_record(ID, StatFeature)
      ; record_profile(StatFeature, ID, Values)
      , output_row(ID, Values)
      ))).

stat_all_deals(StatFeature) :-
  findall(ID, same_deal(ID, _), IDs), 
  list_to_set(IDs, SetIDs), 
  maplist(stat_deal(StatFeature), SetIDs). 

stat_deal(StatFeature, Deal) :-
  deal_profile(StatFeature, Profile), 
  once(deal_records(StatFeature, Deal, Records)),
  match_profile(Records,Profile,SubSet,Hand,Suit,Features), 
  ( memberchk(bids(BidList), Profile)
  , find_mean_imp_diff(SubSet, Hand, BidList, Ns, Imps)
  , flatten([BidList, Ns, Imps, Hand, Suit, Features], 
             Values)
  , output_row(Deal, Values)
  ; \+ memberchk(bids(_), Profile) 
  , find_set_bids(SubSet, Hand, Bids)
  , forall(member(Bid, Bids), 
      ( find_mean_imp_diff(SubSet, Hand, [Bid], Ns, Imps)
      , flatten([Bid, Ns, Imps, Hand, Suit, Features], 
             Values)
      , output_row(Deal, Values)))).
stat_deal(_, _). 

find_set_bids(Set, Hand, Bids) :-
  findall(Bid, 
    ( member(ID, Set)
    , bid(ID, Hand, 1, Bid, _)
    ),
    BidList),
  list_to_set(BidList, Bids). 

find_mean_imp_diff(Set, Hand, BidList, [N1, N2], Imps) :-
  ( [Bid1, Bid2] = BidList 
  , find_records(Set, Hand, Bid1, SubSet1) 
  , find_records(Set, Hand, Bid2, SubSet2)
  ; [Bid] = BidList
  , find_records(Set, Hand, Bid, SubSet1) 
  , find_records_complement(Set, Hand, Bid, SubSet2) 
  ), 
  SubSet1 \= [], SubSet2 \= [], 
  findall(Imps, 
    ( member(ID1, SubSet1)
    , member(ID2, SubSet2)
    , get_imp_score(ID1, ID2, Hand, Imps))
    , ImpSet),
  sum_list(ImpSet, Imps), 
  length(SubSet1, N1),
  length(SubSet2, N2).

find_records(Set, Hand, Bid, SubSet) :-
  findall(ID, 
    ( member(ID, Set)
    , bid(ID, Hand, 1, Bid, _))
    , SubSet).
find_records_complement(Set, Hand, Bid, SubSet) :-
  findall(ID, 
    ( member(ID, Set)
    , \+ bid(ID, Hand, 1, Bid, _))
    , SubSet).

deal_records(StatFaeture, Deal, RecordSet) :-
  findall(ID, same_deal(Deal, ID), IDs), 
  flatten([Deal, IDs], FullSet), 
  findall(ID, 
    ( member(ID,FullSet),suitable_record(ID,StatFaeture)), 
    RecordSet),
  length(RecordSet, L), 
  L > 1. 

% Init & Bookkeeping

init_stat(Dataset, StatFeature, Labels, Stream) :-
  atom_concat(Dataset, '_data.pl', DataFile),
  atom_concat(Dataset, '_errors.pl', ErrorFile),
  ensure_loaded([DataFile, ErrorFile]), 
  atom_concat(StatFeature, '.txt', FileName), 
  open(FileName, write, Stream, [alias(output)]),
  format(output, "id year", []),
  forall(member(Label, Labels),
         format(output, " ~w", [Label])),
  format(output, "~n", []).

output_row(ID, Values) :-
  record_feature(ID, year(Year)), 
  format(output, "~w ~w", [ID, Year]),
  forall(member(Value, Values), format(output," ~w",[Value])),
  format(output, "~n", []).

suitable_record(ID, _) :-
  \+ error(ID, _). 
suitable_record(ID, StatFeature) :-
  findall(F, required_info(F), GeneralFeatures), 
  findall(F, required_info(StatFeature, F), SpecificFeatures), 
  flatten([GeneralFeatures, SpecificFeatures], Required), 
  error(ID, Errors), 
  forall(member(F, Required), \+ member(issue(F, _), Errors)).

required_info(year). 
required_info(hands). 
required_info(auction). 
required_info(contract). 
required_info(declarer). 

% Frequency first hand is opened, independnt of hand type 
% 1=true, 0=false

labels(opening_1st_hand, [bool]). 

record_profile(opening_1st_hand, ID, [Bool]) :-
  bid(ID, 1, 1, _, Features), !,
  ( memberchk(opening, Features)
  , Bool is 1
  ; Bool is 0
  ).  
record_profile(opening_1st_hand, _, ['NA']).


% Frequency of contested auction

labels(contested_auction, [bool,nbids,alength]). 

record_profile(contested_auction, ID, [Bool, NBids, Length]) :-
  findall(Bid, bid(ID, _, _, Bid, _), Bids), 
  split_bids(Bids, DealerSide, OppSide),
  sum_list(DealerSide, Num1),
  sum_list(OppSide, Num2),
  ( Num1 == 0, Num2 == 0, Bool = "NA" 
  ; Num1 > 0, Num2 > 0, Bool is 1     
  ; Bool is 0
  ), 
  record_feature(ID, auction(Auction)), 
  maplist(non_pass_count, Auction, Counts), 
  sum_list(Counts, NBids), 
  length(Auction, Length).

non_pass_count(0, 0).
non_pass_count(_, 1). 

% Frequency balanced (4333/4432/5332) in 1st psotion
% 1=BAL, 0=other

labels(bal_1st_hand, [bool]). 

record_profile(bal_1st_hand, ID, [1]) :-
  hand(ID, 1, _, _, Features),
  memberchk(balanced, Features). 
record_profile(bal_1st_hand, _, [0]).


% Frequency opening BAL 11-12 HCP in 1st hand

labels(opening_11_12_bal_1, [bool]). 

record_profile(opening_11_12_bal_1, ID, [Bool]) :-
  hand(ID, 1, HCP, _, HandFeatures),  
  ( HCP == 11 ; HCP == 12),
  memberchk(balanced, HandFeatures),
  ( bid(ID, 1, 1, _, Features)
  , memberchk(opening, Features)
  , Bool is 1
  ; Bool is 0 ).
record_profile(opening_11_12_bal_1, _, ['NA']).

% Frequency opening BAL 11-12 HCP in 1st/2nd hand

labels(opening_11_12_bal_2, [bool]). 

record_profile(opening_11_12_bal_2, ID, [Bool]) :-
  hand(ID, 1, HCP, _, HandFeatures),  
  ( HCP == 11 ; HCP == 12),
  memberchk(balanced, HandFeatures),
  ( bid(ID, 1, 1, _, Features)
  , memberchk(opening, Features)
  , Bool is 1
  ; Bool is 0 ).
record_profile(opening_11_12_bal_2, ID, [Bool]) :-
  bid(ID, 1, 1, 0, _), 
  hand(ID, 2, HCP, _, HandFeatures),  
  ( HCP == 11 ; HCP == 12),
  memberchk(balanced, HandFeatures),
  ( bid(ID, 2, 1, _, Features)
  , memberchk(opening, Features)
  , Bool is 1
  ; Bool is 0 ). 
record_profile(opening_11_12_bal_2, _, ['NA']).


% Frequency of preempts

labels(preempt, [lvalue]). 

record_profile(preempt, ID, [Diff]) :-
  is_preempt(ID, 1, Level, Length),
  Diff is Length - Level, 
  ( Diff > 1 
  ; format("Not included: Deal ~w, diff is ~w~n", [ID, Diff])
  , fail
  ). 
record_profile(preempt, _, [0]). 

is_preempt(_, 5, _, _) :- !, fail.  
is_preempt(ID, Hand, Level, Length) :-
  hand(ID, Hand, HCP, [_, [Length|_]], _),
  HCP < 10, 
  bid(ID, Hand, 1, Bid, Features),
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

labels(weak_two_1st, [level]). 
labels(weak_two_1st_nv, [level]). 

record_profile(weak_two_1st, ID, [Value]) :-
  hand(ID, 1, HCP, [_, [6|_]], _), 
  HCP < 10, 
  bid(ID, 1, 1, Bid, _), 
  Value is Bid div 10. 
record_profile(weak_two_1st, _, ["NA"]).

record_profile(weak_two_1st_nv, ID, [Value]) :-
  record_feature(ID, vul(Vul)),
  ( Vul == 1 ; Vul == 3), 
  hand(ID, 1, HCP, [_, [6|_]], _), 
  HCP < 10, 
  bid(ID, 1, 1, Bid, _), 
  Value is Bid div 10. 
record_profile(weak_two_1st_nv, _, ["NA"]).
    
% With how many points is a game bid, and is it made
% We look only at undoubled games, to remove saves amap

labels(game_hcp, [game, gtype, vul, hcp, made]). 

record_profile(game_hcp, ID, [1, GameType, IsVul, HCP, Made]) :-
  record_feature(ID, contract([Level, Suit])), 
  is_game([Level, Suit], GameType), 
  record_feature(ID, declarer(Declarer)), 
  record_feature(ID, vul(DealVul)), 
  is_vul(Declarer, DealVul, IsVul), 
  the_partner(Declarer, Partner), 
  hand(ID, Declarer, HCP1, _, _), 
  hand(ID, Partner, HCP2, _, _),
  HCP is HCP1 + HCP2,
  record_feature(ID, result(Result)), 
  ( Result < Level + 6
  , Made is 0
  ; Made is 1). 
record_profile(game_hcp, _, [0, 'NA','NA','NA','NA']).
 
is_game([3,n], nt).
is_game([4,n], nt).
is_game([5,n], nt).
is_game([4,h], ma).
is_game([4,s], ma).
is_game([5,h], ma).
is_game([5,s], ma).
is_game([5,c], mi).
is_game([5,d], mi).

the_partner(1, 3).
the_partner(2, 4).
the_partner(3, 1).
the_partner(4, 2).

% Profile: Muiderberg

deal_profile(mbs, 
  [ labels([bid, n1, n2, imps, hand, suit, hcp, suitq])
  , hcp([5, 10]) 
  , main_suit([spades, 5, =])
  , suit([minor, 4, +])
  , auction([ [], [0], [0,0] ])
  ]). 
deal_profile(mbh, 
  [ labels([bid, n1, n2, imps, hand, suit, hcp, suitq])
  , hcp([5, 10]) 
  , main_suit([hearts, 5, =])
  , suit([minor, 4, +])
  , auction([ [], [0], [0,0] ])
  ]). 

deal_profile(mbs024, 
  [ labels([bid1,bid2,n1,n2,imps,hand,suit,hcp,suitq])
  , bids([24, 0])
  , hcp([5, 10]) 
  , main_suit([spades, 5, =])
  , suit([minor, 4, +])
  , auction([ [], [0], [0,0] ])
  ]). 
deal_profile(mbh023, 
  [ labels([bid1,bid2,n1,n2,imps,hand,suit,hcp,suitq])
  , bids([23, 0])
  , hcp([5, 10]) 
  , main_suit([hearts, 5, =])
  , suit([minor, 4, +])
  , auction([ [], [0], [0,0] ])
  ]). 

% Profiles: weak two

deal_profile(wts, 
  [ labels([bid, n1, n2, imps, hand, suit, hcp, suitq])
  , hcp([5, 9]) 
  , main_suit([spades, 6, =])
  , feature([single_suited, two_suited_short])
  , auction([ [], [0], [0,0] ])
  ]). 
deal_profile(wth, 
  [ labels([bid, n1, n2, imps, hand, suit, hcp, suitq])
  , hcp([5, 9]) 
  , main_suit([hearts, 6, =])
  , feature([single_suited, two_suited_short])
  , auction([ [], [0], [0,0] ])
  ]). 

deal_profile(wts2224, 
  [ labels([bid1,bid2,n1,n2,imps,hand,suit,hcp,suitq])
  , bids([22, 24])
  , hcp([5, 9]) 
  , main_suit([spades, 6, =])
  , feature([single_suited, two_suited_short])
  , auction([ [], [0], [0,0] ])
  ]). 
deal_profile(wth2223, 
  [ labels([bid1,bid2,n1,n2,imps,hand,suit,hcp,suitq])
  , bids([22, 23])
  , hcp([5, 9]) 
  , main_suit([hearts, 6, =])
  , feature([single_suited, two_suited_short])
  , auction([ [], [0], [0,0] ])
  ]). 

% Profiles: opening 1NT with 5M

deal_profile(nt5s, 
  [ labels([bid1, bid2, n1, n2, imps, hand, suit, hcp, suitq])
  , bids([14, 15])
  , hcp([14,15]) 
  , main_suit([spades, 5, =])
  , feature([balanced])
  , auction([ [], [0], [0,0] ])
  ]). 
deal_profile(nt5h, 
  [ labels([bid1, bid2, n1, n2, imps, hand, suit, hcp, suitq])
  , bids([13, 15])
  , hcp([14,15]) 
  , main_suit([hearts, 5, =])
  , feature([balanced])
  , auction([ [], [0], [0,0] ])
  ]). 

% Auxilliary predicates for statistics

split_bids([B1,B2,B3,B4,B5,B6,B7,B8],[B1,B3,B5,B7],[B2,B4,B6,B8]).
split_bids([B1,B2,B3,B4,B5,B6,B7],[B1,B3,B5,B7],[B2,B4,B6]).
split_bids([B1,B2,B3,B4,B5,B6],[B1,B3,B5],[B2,B4,B6]).
split_bids([B1,B2,B3,B4,B5],[B1,B3,B5],[B2,B4]).
split_bids([B1,B2,B3,B4],[B1,B3],[B2,B4]).
split_bids([B1,B2,B3],[B1,B3],[B2]).
split_bids([B1,B2],[B1],[B2]).
split_bids([B1],[B1],[]).
split_bids([],[],[]).   

  
