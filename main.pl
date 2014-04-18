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
:- discontiguous(statistics/3).  

:- ensure_loaded(
      [ process
      , definitions
      , statistics
      , errors
      ]). 

load_db(DB) :-
  concat_atom([DB, '_db.pl'], Deals), 
  ensure_loaded(Deals),
  defs.
