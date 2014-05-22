/*  File        main.pl
 *  Part of     bidstat: analysis of bridge bidding 
 *  Author      Guus Schreiber, guus.schreiber@vu.nl
 *  Works with  SWI-Prolog 7.1.3
 */


/*------------------------------------------------------------
 *  Main
 *------------------------------------------------------------*/

:- dynamic(deal/2). 
:- dynamic(hand/5). 
:- dynamic(bid/5). 
:- dynamic(required_info/1). 

:- discontiguous(stat_feature/1). 
:- discontiguous(deal_profile/3). 
:- discontiguous(set_profile/2). 
:- discontiguous(required_info/2). 
:- discontiguous(labels/2). 
:- discontiguous(deal/2). 
:- discontiguous(hand/5). 
:- discontiguous(bid/5). 
:- discontiguous(process_feature/3). 

:- ensure_loaded(
      [ aux
      , process
      , definitions
      , check
      , profile
      , statistics
      ]). 


test(Dataset) :-
  atomic_list_concat(['pbn/', Dataset, '/'], Path), 
  process_dataset(test, [[Path, Dataset]]),
  check_dataset(test).  

exp1 :-
  process_dataset(exp1, [ 
     ['pbn/bb/', bb] 
     ]),
  check_dataset(exp1). 

exp2 :-
  process_dataset(exp2, [ 
     ['pbn/bb/', bb9], 
     ['pbn/ca/', ca],
     ['pbn/dt/', dt],
     ['pbn/ec/', ec], 
     ['pbn/ol/', ol], 
     ['pbn/sg/', sg], 
     ['pbn/us/', us], 
     ['pbn/vb/', vb]
     ]),
  check_dataset(exp2). 

run_exp(Experiment) :-
  forall(stat_feature(Feature), stat(Experiment, Feature)). 

% Deal profiles

stat_feature(opening_1st_hand).
stat_feature(contested_auction). 
stat_feature(bal_1st_hand). 
stat_feature(opening_11_12_bal_2). 
stat_feature(preempt). 
stat_feature(weak_two_1st).
stat_feature(weak_two_1st_nv).
stat_feature(game_hcp). 

% Set profiles

stat_feature(wts). 
stat_feature(wth). 
stat_feature(wts2224). 
stat_feature(wth2223). 
stat_feature(mbs). 
stat_feature(mbh). 
stat_feature(mbs024). 
stat_feature(mbh023).
stat_feature(nt5s). 
stat_feature(nt5h). 



