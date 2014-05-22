print_deal(ID) :-
  print_deal_data(ID, D), 
  print_hands(ID, D), 
  newline, 
  findall(ID2, same_deal(ID, ID2), Members),
  flatten([ID, Members], Set), 
  maplist(print_record(D), Set). 

print_deal_data(ID, Dealer) :-
  record_feature(ID, event(Event)), 
  format("Event: ~s~n", [Event]),
  record_feature(ID, dealer(Dealer)),  
  record_feature(ID, vul(Vul)),
  get_dealer(Dealer, D), 
  get_vul(Dealer, Vul, V), 
  format("~a/~a~n", [D,V]).

get_dealer(n, 'N'). 
get_dealer(e, 'E'). 
get_dealer(s, 'S'). 
get_dealer(w, 'W').

get_vul(_, 1, '-'). 
get_vul(_, 4, 'All').
get_vul(n, 2, 'NS').
get_vul(n, 3, 'EW').
get_vul(s, 2, 'NS').
get_vul(s, 3, 'EW').
get_vul(e, 3, 'NS').
get_vul(e, 2, 'EW').
get_vul(w, 3, 'NS').
get_vul(w, 2, 'EW').
 
print_hands(ID, Dealer) :-
  record_feature(ID, hands(Hands)),
  hand_order(Dealer, Order), 
  hand_labels(Labels), 
  maplist(print_hand(Hands), Order, Labels). 

hand_order(n, [1,2,3,4]).
hand_order(e, [4,1,2,3]).
hand_order(s, [3,4,1,2]).
hand_order(w, [2,3,4,1]).

hand_labels(
  [ 'North: '
  , 'East:  '
  , 'South: '
  , 'West:  ']). 

print_hand(Hands, Num, Label) :-
  nth1(Num, Hands, Hand),
  format("~a", [Label]),  
  maplist(print_suit, Hand), 
  newline. 

print_suit(Suit) :-
  ( Suit == []
  , format("-", [])
  ; forall(member(Code, Suit), 
    ( card_code(Code, Card)
    , format("~a", [Card])))
  ), 
  format(" ", []).

card_code(a, 'A').
card_code(k, 'K').
card_code(q, 'Q').
card_code(j, 'J').
card_code(t, 'T').
card_code(C, C).
 
print_record(Dealer, ID) :-
  format("Deal record: ~a~n", [ID]), 
  record_feature(ID, auction(A)),
  record_feature(ID, players(Ps)),
  record_feature(ID, contract(C)), 
  f_contract(C, Contract), 
  record_feature(ID, declarer(D)),
  f_declarer(Dealer, D, Declarer),  
  record_feature(ID, result(Result)), 
  record_feature(ID, score(S)), 
  f_score(Dealer, S, Score),  
  print_auction(Dealer, Ps, A), 
  format("Contract: ~a ; Declarer: ~a ; Tricks: ~a ; Score NS: ~a~n", 
         [Contract, Declarer, Result, Score]),
  newline.

f_declarer(_, 0, '-'). 
f_declarer(n, 1, 'N').
f_declarer(n, 2, 'E').
f_declarer(n, 3, 'S').
f_declarer(n, 4, 'W').
f_declarer(e, 4, 'N').
f_declarer(e, 1, 'E').
f_declarer(e, 2, 'S').
f_declarer(e, 3, 'W').
f_declarer(s, 3, 'N').
f_declarer(s, 4, 'E').
f_declarer(s, 1, 'S').
f_declarer(s, 2, 'W').
f_declarer(w, 2, 'N').
f_declarer(w, 3, 'E').
f_declarer(w, 4, 'S').
f_declarer(w, 1, 'W').

f_score(D, S, S) :-
  ( D == n ; D == s ).
f_score(D, S, Score) :-
  ( D == e ; D == w ), 
  Score is -1 * S. 

f_contract(pass, pass). 
f_contract([L, S, DR], CDR) :-
  f_contract([L, S], C), 
  atom_concat(C, DR, CDR).
f_contract([L, S], C) :-
  f_suit(S, Suit), 
  atom_concat(L, Suit, C).

f_suit(c, 'C').
f_suit(d, 'D').
f_suit(h, 'H').
f_suit(s, 'S').
f_suit(n, 'NT').

print_auction(Dealer, Players, Auction) :-
  hand_labels(Labels),
  maplist(print_player, Players, Labels),
  format("North   East    South   West~n"),
  print_auction_lines(Dealer, Auction). 

print_player(Player, Label) :-
  format("    ~a~a~n", [Label, Player]).

print_auction_lines(Dealer, Auction) :-
  add_hyphens(Dealer, Auction, A),
  print_auction_line(A).

add_hyphens(n, A, A).
add_hyphens(e, A, [n | A]).
add_hyphens(s, A, [n, n | A]).
add_hyphens(w, A, [n, n, n |A]).

print_auction_line(A) :-
  length(A, L), 
  L =< 4, 
  maplist(print_bid, A),
  newline.
print_auction_line([B1, B2, B3, B4 | Rest]) :-
  maplist(print_bid, [B1, B2, B3, B4]),
  newline, 
  print_auction_line(Rest). 

print_bid(BidCode) :-
  code_to_bid(BidCode, Bid), 
  format("~a    ", [Bid]). 

code_to_bid(n, '-   '). 
code_to_bid(0, 'pass'). 
code_to_bid(7, 'X   '). 
code_to_bid(8, 'XX  '). 
code_to_bid(Num, Bid) :-
  Num > 10, 
  Level is Num div 10, 
  Suit is Num mod 10, 
  suit_label(Suit, SuitLabel),
  atom_concat(Level, SuitLabel, Bid).

suit_label(1, 'C  ').
suit_label(2, 'D  ').
suit_label(3, 'H  ').
suit_label(4, 'S  ').
suit_label(5, 'NT ').

newline :-
  format("~n", []).
