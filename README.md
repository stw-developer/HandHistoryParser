# HandHistoryParser Suite

The HandHistoryParser suite is a lightweight poker tracking software application that allows users to track their online poker performance based on their playing history. Players input their hand history files, and the program outputs key statistics on their play (performance/profitability) in both tabular and graphical formats.

Typical Poker tracking software is heavy in nature providing the user with a large number of statistics and customisable analytics . Such software is aimed at professional poker players and usually comes at non-trivial cost to the user.

The HandHistoryParser suite aims to provide a light-weight altnernative that provides the player with only the fundamental stats widely used for measuring poker performance. For this reason HandHistoryParser would be aimed at the casual or novice player who would be interested in getting a top-level gauge of their performance; or for players who just want a simple and easy to use poker tracker.

The fundamental stats produced by HandHistoryParser are:

bb/100 [big blinds per 100 hands]
profit expressed as number of big blinds (standard bets) per 100 hands.

VPIP [Voluntary put $ in pot]
% of time you are making pre-flop raises or calls. Measure of playing style (tight, loose, etc)

PFR  [Pre Flop Raise]
% of time you are making pre-flop raises. Measure of player aggressiveness.

In addition to the fundamentals, a few other high level stats are produced. These are

- hands (dealt, played, won) in absolute and % terms 
- net profit
- average profit
- 100-hand average profit 

Output consists of 
- Statistics (performance, profitability and per hand) printed to screen.
- graphs that show running profit balances (profits vs hands played).
- hand by hand analysis file (csv) - this can be toggled on and off.

The suite consists of three main files:

1. hh_parser.r        - parses raw hand history files and produces a matrix of bets suitable for analysis.
2. hh_analysis.r      - performs analysis on bet matrix from hh_parser and outputs stats and graphs.
3. hh_parser_config.r - config file allowing user to specify program behaviour (input/output dirs, read/write options, etc)

Notes:
02/08/2017 
- Currently only analysis of PokerStars.com hand histories are supported.
- Analysis only for cash games. Aim to support tournaments in the near future. 

#################################################################
# hh_analysis.r - poker hand history analyser
# hand history analyzer for PokerStars
# analyzes handhistory text file from pokerstars and
# saves to csv. Also outputs Running profit balance, profit/hand
# running profit graphs.
#
# Calculates the following:
# - Hand statistics (hands (dealt, played, won) in absolute and % terms)
# - Fundamental (bb/100, VPIP, PFR)
# - Profitability (net profit, average profit, 100-hand average) 
#
# Author: sherhan wicky
# Date  : 30/04/2017
# Ver   : 0.1

#################################################################
# hh_parser.r - poker hand history parser
# hand history parsing script for PokerStars
# parses handhistory text file from pokerstars and
# saves to csv. Currently only processes cash games.
#
# Author: sherhan wicky
# Date  : 19/04/2017
# Ver   : 0.1

