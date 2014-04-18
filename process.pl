/*  File        process.pl
 *  Part of     bidstat: analysis of bridge bidding 
 *  Author      Guus Schreiber, guus.schreiber@vu.nl
 *  Purpose     process output of Perl script pbn2prolog
 *  Works with  SWI-Prolog 7.1.3
 */


/*------------------------------------------------------------
 *  Process raw deal information
 *------------------------------------------------------------*/

/* 
  This code reads the output of pbn2prolog and transforms the
  relevant data in computation-friendly format 
 
  Main predicate: "process(<File>)"
    where <File> is the name of the pbn2prolog file without 
    extension output is a file <File>_db.pl with facts
    of type "deal(ID, Features)", where ID is a gensymed key
    and "Features" is a list of 8 features, namely:
      1. Year
      2. Hand: list of 4 hands, first hand is the dealer 
         example hand: [[a,k,q,j],[t,9,8],[7,6,5],[4,3,2,]]
      3. Vulnerablility: 1 = none, 2 = dealer-side V, etc
      4. Scoring method: 1 for "imps", 0 for other
      5. Auction: list of bids
         bid representation: 0=pass, 7/8=dbl/rdbl, 11=1C, 
         12=1D, 13=1H, 14=1S, 15=1NT, 21=2C, etc
      6. Contract: list of 2 or 3 elemanys, e.g. [3,d,x]
      7. Declarer: 1-4 (in order of hands; dealer = 1)
      8. Result: #tricks, 0 if passed out
      NB: - NS/EW information is not preserved (not needed).
          - score info not preserved
*/

process(FileRoot) :-
  retractall(deal/2),
  retractall(hand/5),
  retractall(bid/5),
  reset_gensym(deal_),
  concat_atom([FileRoot, '.pl'], Raw),
  concat_atom([FileRoot, '_db.pl'], DB),
  concat_atom([FileRoot, '_error.pl'], ErrorFile),
  open(Raw, read, ReadStream, [alias(input)]), 
  open(DB, write, WriteStream, [alias(output)]), 
  open(ErrorFile, write, ErrorStream, [alias(error)]), 
  read(input, Deal), 
  process_deal(Deal),
  close(ReadStream),
  close(WriteStream),
  close(ErrorStream).


process_deal(end_of_file).
process_deal(deal([end_of_deal])) :-
  read(input, Deal), 
  process_deal(Deal).
process_deal(deal(Args)) :-  
  process_args(Args, Facts),
  gensym(deal_, ID),
  write_term(output, deal(ID, Facts), [fullstop(true), nl(true)]),
  read(input, Deal),
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
  ; write_ln(error, "Error: no Dealer")
  , fail
  ), !.  

nsew(n).
nsew(e).
nsew(s).
nsew(w).

process_year(Args, Year) :-
  ( memberchk(tag('Date', [Year|_]), Args)
  ; format(error, "Error: no Year ~n ~w ~n", [Args])
  , fail
  ), !.

process_hand(Args, Dealer, H) :-
  ( memberchk(tag('Deal', Hand), Args)
  , \+ var(Hand)
  , process_h(Dealer, Hand, H)
  ; format(error, "Error: no Hand ~n ~w ~n", [Args])
  , fail
  ), !.
 
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

process_vul(Args, Dealer, Vul) :-
  memberchk(tag('Vulnerable', V), Args),
  ( \+ var(V)
  , process_v(Dealer, V, Vul)
  ; format(error, "Error: no Vulnerability ~n ~w ~n", [Args])
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
  format(error, "Error: no Auction ~n ~w ~n", [Args]), 
  fail, !. 
process_auction(Args, Dealer, Auction) :-
  memberchk(tag('Auction', Hand), Args), 
  ( Dealer == Hand
  ; format(error, "Warning: dealer/auction mismatch: ~n ~w ~n", [Args])
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

