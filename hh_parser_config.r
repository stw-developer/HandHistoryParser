#Player to analyze
PLAYER_NAME     <- "the_wickstar"

#Write to file flag
WRITE_PARSED_FLAG <- FALSE
WRITE_ANALYSIS_FLAG <- FALSE

#Use DATA_FILE as input file if SINGLE_FILE flag set to TRUE,
#otherwise use all *.txt files found in DATA_DIR.
SINGLE_FILE <- FALSE

#Include/Exclude (TRUE/FALSE) tournament hands (default exclude)
TOURNAMENT_HANDS <- FALSE

#Input/Output filepaths
DATA_FILE    	<- "./data/pokerstars_20170505_2.txt"
PARSED_FILE 	<- "./parsed/HandTableOutput2.csv"
ANALYSIS_FILE   <- "./analysis/HandAnalysis.csv"
DATA_DIR     	<- "./data/"
PARSE_DIR    	<- "./parsed/"

#Analysis variables
WINDOW_SIZE  	<- 100  
