/*  File        process.pl
 *  Part of     bidstat: analysis of bridge bidding 
 *  Author      Guus Schreiber, guus.schreiber@vu.nl
 *  Purpose     process output of Perl script pbn2prolog
 *  Works with  SWI-Prolog 7.1.3
 */


/*----------------------------------------------- 
 *  Process raw deal record information
 *------------------------------------------------*/

/* 
  This code reads the output of pbn2prolog and transforms the
  relevant data in computation-friendly format 
 
  Main predicate: process(<Path>, <File>)
    where <File> is the name of the pbn2prolog file without 
    extension output is a file <File>_data.pl with facts
    of type deal_record(ID, Features), where ID is a gensymed key
    and Features is a list of features, including:
      *  dealer(): one of: {n,e,s,w}
      *  year()
      *  hands(H): list of 4 hands, first hand is the dealer 
         example hand: [[a,k,q,j],[t,9,8],[7,6,5],[4,3,2,]]
      *  vul(V): 1 = none, 2 = dealer-side V, etc
      *  auction(A): list of bids
         bid representation: 0=pass, 7/8=dbl/rdbl, 11=1C, 
         12=1D, 13=1H, 14=1S, 15=1NT, 21=2C, etc
      *  contract(C): list of 2 or 3 elemanys, e.g. [3,d,x]
      *  declarer(D): 1-4 (in order of hands; dealer = 1)
      *  result(R): number of tricks, 0 if passed out
      NB: - full NS/EW information is not preserved (not needed).
          - score info not preserved (= recomputed)
*/

:- dynamic(hands/2). 

process_dataset(Dataset, Collections) :-
  atom_concat(Dataset, '_data.pl', DataFile),
  open(DataFile, write, DataStream, [alias(data)]), 
  maplist(process, Collections, Totals), 
  sum_list(Totals, Overall),
  find_same_deals, 
  format(user, "Total number of deal records processed: ~a~n", 
    [Overall]),  
  close(DataStream).

find_same_deals :-
  findall(ID, deal_record(ID, _), IDs), 
  maplist(find_same_deal, IDs).

find_same_deal(ID1) :-
  \+ record_feature(ID1, hands(_)).
find_same_deal(ID1) :-
  record_feature(ID1, hands(H)),
  ( hands(ID2, H)
  , write_term(data, same_deal(ID2, ID1), [fullstop(true), nl(true)])
  , assert(same_deal(ID2, ID1))
  ; asserta(hands(ID1, H))
  ). 

record_feature(ID, Feature) :-
  deal_record(ID, Facts),
  member(Feature, Facts). 

process([Path, Collection], Total) :-
  atomic_list_concat([Path, Collection, '.pl'], Data),
  open(Data, read, ReadStream, [alias(input)]), 
  read(input, Record), 
  process_deal_record(Collection, Record, 0, Total),
  format(user, "Collection ~w processed: ~a deal records~n", 
         [Collection, Total]), 
  close(ReadStream).

process_deal_record(_, end_of_file, Total, Total).
process_deal_record(Collection, record([end_of_record]), ID, Total) :-
  read(input, Record), 
  process_deal_record(Collection, Record, ID, Total).
process_deal_record(Collection, record(Args), PrevID, Total) :-
  ID is PrevID + 1,   
  process_args(Args, Facts),
  atomic_list_concat([Collection, '_', ID], RecordID),
  ( \+ ground(del_record(RecordID, Facts))
  , format(user, "Not ground: ~w ~w ~n", [RecordID, Facts])
  ; assert(deal_record(RecordID, Facts))
  , portray_clause(data, deal_record(RecordID, Facts))
  , deal_record_def(RecordID)
  ), 
  read(input, Record),
  process_deal_record(Collection, Record, ID, Total).

process_args(Args, RecordProps) :-
  findall(F, include_feature(F), Features), 
  maplist(process_feature(Args), Features, Values), 
  flatten(Values, FeatureValues), 
  findall(F, include_derivation(F), Derivations), 
  maplist(process_derivation(FeatureValues), Derivations, DerivedValues), 
  flatten([FeatureValues, DerivedValues], RecordProps).

include_feature(event).
include_feature(players).
include_feature(dealer).
include_feature(year). 
include_feature(hands).
include_feature(vul).
include_feature(auction).
include_feature(contract). 
include_feature(declarer).
include_feature(result). 

include_derivation(score). 

process_feature(Args, event, event(Event)) :-
  memberchk(tag('Event', E), Args), 
  atom_string(Event, E). 

process_feature(Args, players, players([N, E, S, W])) :-
  memberchk(tag('North', North), Args),
  atom_string(N, North), 
  memberchk(tag('East', East), Args), 
  atom_string(E, East), 
  memberchk(tag('South', South), Args), 
  atom_string(S, South), 
  memberchk(tag('West', West), Args),
  atom_string(W, West).

process_feature(Args, dealer, dealer(Dealer)) :-
  memberchk(tag('Dealer', Dealer), Args), 
  nsew(Dealer).

nsew(n).
nsew(e).
nsew(s).
nsew(w).

process_feature(Args, year, year(Year)) :-
  memberchk(tag('Date', [Year|_]), Args).

process_feature(Args, hands, hands(H)) :-
  memberchk(tag('Deal', Hand), Args),  
  process_feature(Args, dealer, dealer(Dealer)), 
  process_h(Dealer, Hand, H). 
 
process_h(Dealer, [Dealer | Hand ], Hand). 
process_h(Dealer, [First | HandList], Hand) :-
  distance(First, Dealer, Distance),
  transpose(Distance, HandList, Hand).

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

process_feature(Args, vul, vul(Vul)) :-
  memberchk(tag('Vulnerable', V), Args),
  process_feature(Args, dealer, dealer(Dealer)), 
  process_v(Dealer, V, Vul).

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

process_feature(Args, scoring, scoring(Scoring)) :-
  memberchk(tag('Scoring', S), Args),
  process_s(S, Scoring), !.
process_scoring(_, 1). 

process_s('IMP', 1).
process_s('IMP_1948', 1).
process_s('IMP_1961', 1).

process_feature(Args, auction, auction(Auction)) :-
  memberchk(tag('Auction', Hand), Args), 
  process_feature(Args, dealer, dealer(Dealer)), 
  Dealer == Hand,
  find_bids(Args, BidList), 
  map_bids(BidList, BidCodeList),
  ( forall(member(Bid, BidCodeList), Bid==0)
  , Auction = [0, 0, 0, 0]
  ; three_end_passes(BidCodeList, Auction)
  ). 

three_end_passes(Auction, CorrectedAuction) :-
  reverse(Auction, RevAuction),
  remove_pass(RevAuction, StrippedRevAuction),
  reverse([0, 0, 0 | StrippedRevAuction], CorrectedAuction). 

remove_pass([], []). 
remove_pass([0 | Rest], Auction) :-
  remove_pass(Rest, Auction). 
remove_pass(Auction, Auction). 

find_bids([br(Bids)|RestArgs], Auction) :-
  find_bids(RestArgs, RestBids), 
  append(Bids, RestBids, Auction).
find_bids([_|RestArgs], Auction) :-
  find_bids(RestArgs, Auction). 
find_bids([], []). 

map_bids([], []).
map_bids([Bid | RestBids], [Code | RestCodes]) :-
  bid_to_num(Bid, Code),
  map_bids(RestBids, RestCodes). 
map_bids([_| RestBids], RestCodes) :-
  map_bids(RestBids, RestCodes). 

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

process_feature(Args, contract, contract(Contract)) :-
  memberchk(tag('Contract', Contract), Args). 

process_feature(Args, declarer, declarer(Declarer)) :-
  memberchk(tag('Declarer', DeclarerNSEW), Args),
  process_feature(Args, contract, contract(Contract)), 
  process_feature(Args, dealer, dealer(Dealer)), 
  process_d(Dealer, Contract, DeclarerNSEW, Declarer).  

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

process_feature(Args, result, result(R)) :-
  memberchk(tag('Result', Result), Args),
  process_feature(Args, contract, contract(Contract)), 
  process_r(Result, Contract, R). 

process_r(_, pass, 0). 
process_r(R, _, R) :-
  integer(R).  

process_feature(_, _, []). 

process_derivation(Features, score, score(0)) :-
  memberchk(contract(pass), Features).

process_derivation(Features, score, score(Score)) :-
  memberchk(result(Tricks), Features), 
  memberchk(contract(Contract), Features), 
  memberchk(declarer(Declarer), Features), 
  declarer_vul(Features, IsVul),
  nth1(1, Contract, Level), 
  Result is Tricks - Level - 6, 
  ( Result < 0
  , process_score_down(Contract, Result, IsVul, DeclScore)
  ; process_score_made(Contract, Result, IsVul, DeclScore)
  ), 
  declarer_to_dealer(Declarer, PlusOrMinus),
  Score is PlusOrMinus * DeclScore.
process_derivation(_, _, []). 
 
declarer_to_dealer(1, 1).
declarer_to_dealer(2, -1).
declarer_to_dealer(3, 1).
declarer_to_dealer(4, -1).

process_score_made([Level, Suit], OverTricks, IsVul, Score) :-
  contract_value(Level, Suit, CValue), 
  overtricks(OverTricks, Suit, OValue), 
  once(premium(CValue, Level, IsVul, Premium)),
  Score is CValue + OValue + Premium. 
process_score_made([Level, Suit, DblOrRdbl], OverTricks, IsVul, Score) :-
  contract_value(Level, Suit, DblOrRdbl, CValue), 
  overtricks(OverTricks, DblOrRdbl, IsVul, OValue), 
  once(premium(CValue, Level, IsVul, Premium)),
  Score is CValue + OValue + Premium + 50. 

contract_value(Level, Suit, CValue) :-
  trick_value(Suit, SValue), 
  ( Suit == n
  , CValue is 10 + (Level * SValue)
  ; CValue is Level * SValue
  ). 

contract_value(Level, Suit, x, DCValue) :-
  contract_value(Level, Suit, CValue),
  DCValue is CValue * 2. 
contract_value(Level, Suit, xx, DCValue) :-
  contract_value(Level, Suit, CValue),
  DCValue is CValue * 4. 

overtricks(OverTricks, Suit, OValue) :-
  trick_value(Suit, SValue), 
  OValue is OverTricks * SValue. 

overtricks(OverTricks, x, no, OValue) :-
  OValue is OverTricks * 100. 
overtricks(OverTricks, x, yes, OValue) :-
  OValue is OverTricks * 200.
overtricks(OverTricks, xx, no, OValue) :-
  OValue is OverTricks * 200. 
overtricks(OverTricks, xx, yes, OValue) :-
  OValue is OverTricks * 400. 

premium(CValue, _, _, 50) :-
  CValue < 100. 
premium(_, 6, no, 800).
premium(_, 6, yes, 1250).
premium(_, 7, no, 1300).
premium(_, 7, yes, 2000).
premium(_, _, no, 300).
premium(_, _, yes, 500).

trick_value(c, 20).
trick_value(d, 20).
trick_value(h, 30).
trick_value(s, 30).
trick_value(n, 30).

process_score_down([_, _], Down, IsVul, Score) :-
  ( IsVul == no
  , Score is 50 * Down
  ; Score is 100 * Down
  ). 

process_score_down([_, _, x], -1, no, -100).
process_score_down([_, _, x], -2, no, -300).
process_score_down([_, _, x], Down, no, Score) :-
  VulDown is Down + 1,
  process_score_down([_, _, x], VulDown, yes, Score).
process_score_down([_, _, x], Down, yes, Score) :-
  ExtraDown is Down + 1,
  Score is -200 + ( ExtraDown * 300). 
process_score_down([_, _, xx], Down, no, Score) :-
  ExtraDown is Down + 1,
  Score is -200 + ( ExtraDown * 400). 
process_score_down([_, _, xx], Down, yes, Score) :-
  ExtraDown is Down + 1,
  Score is -400 + ( ExtraDown * 600). 

declarer_vul(Features, IsVul) :-
  memberchk(declarer(D), Features),
  memberchk(vul(V), Features),
  is_vul(D, V, IsVul).

is_vul(_, 1, no). 
is_vul(_, 4, yes). 
is_vul(1, 2, yes). 
is_vul(1, 3, no). 
is_vul(2, 2, no). 
is_vul(2, 3, yes). 
is_vul(3, 2, yes). 
is_vul(3, 3, no). 
is_vul(4, 2, no). 
is_vul(4, 3, yes). 
 
