#################################################################
# hh_parser.r - poker hand history parser
# hand history parsing script for PokerStars
# parses handhistory text file from pokerstars and
# saves to csv. Currently only processes cash games.
#
# Author: sherhan wicky
# Date  : 19/04/2017
# Ver   : 0.1

########### LIBRARIES    #########################################
source("hh_parser_config.r")

########### GLOBAL VARS ##########################################

#Length of the handID required for regexp matching
HAND_ID_LENGTH <- 11

#Row references for hands
HANDNUM_ROW     <- 2;
DATETIME_ROW    <- 3;
TABLENAME_ROW   <- 4;

############ FUNCTIONS   ##########################################

# loads hand data from text file into a list
import_hands <- function(filepath) {
	# Load hand history raw textfile
	textfile <- readLines(filepath);

	# Get row number of starting header for each hand
	hand_start_rows <- grep("# [0-9]", textfile);

	# Get number of hands
	numHands <- length(hand_start_rows);

	# Get number of rows for each hand
	lasthandRows <- length(textfile)-hand_start_rows[numHands]
	rowsPerHand <- c(diff(hand_start_rows), lasthandRows);

	# Split textfile into a list of hands. i.e. hand[1] = hand 1 rows.
	firstHandRow <- hand_start_rows[1]
	textfile_noheader <- textfile[firstHandRow:length(textfile)]    
	numGroups <- length(rowsPerHand);

	#return list of hands
	handlist <- split(textfile_noheader, rep(1:numGroups, rowsPerHand));
}

# get handIDs for each hand in handlist. handlist is a list of character vectors
# representing individual PokerStars hands. id_length represents length of unique
# handId	
# Returns a character vector of HandIDs one for each hand in handlist.
get_handIDs <- function(handlist, id_length) {
	pattern <- paste("[^ #][0-9]{",id_length,"}",sep="")
	match 	<- regexpr(pattern, handlist,  perl=TRUE)
        found 	<- regmatches(as.character(handlist), match)
}

############ DATA IMPORT ##########################################

# get input filenames
if(SINGLE_FILE == TRUE) {
	data_files <- DATA_FILE
	numFiles   <- length(data_files) 
} else {
	data_files <- list.files(DATA_DIR,pattern="*.txt")
	data_files <- paste(DATA_DIR,data_files, sep="")
	numFiles   <- length(data_files) 
}

# import each file in DATA_DIR
hands <- list()

for (i in 1:numFiles) {
	hands <- c(hands, import_hands(data_files[i]))
	print(paste("Imported data file: ", data_files[i]))
}

#remove tournament hands if flag set
if (TOURNAMENT_HANDS == FALSE) {
	tourn_elements <- grep("Tournament", hands)
	if(length(tourn_elements) > 0) {
		hands <- hands[-c(tourn_elements)]
	}
}

# remove duplicate hands
handIDList	<- get_handIDs(hands, HAND_ID_LENGTH)
dupe_pos	<- duplicated(handIDList) 
hands		<- hands[!dupe_pos]	  

# get number of hands
numHands <- length(hands)


############ PARSING ####################################################

# Define hand table columns
# hand table contains top level hand information (table, time,gametype)
# etc
HandID <- numeric(numHands);
Timestamp   <- character(numHands);
TableName <-character(numHands);
GameType <-character(numHands);
HoleCard1 <- character(numHands);
HoleCard2 <- character(numHands);

#Players <- rep(list(list()), numHands);
Players  <- list();

# Counter init
j <- 1;
k <- 1;

# BET MATRIX init - table containing instances where players bet.
# folds not included.
labels     <- c("HandID","LegType","Player","Action","Bet")
BetMatrix  <- matrix(NA, numHands*200, length(labels))
colnames(BetMatrix) <- labels

#LEG MATRIX init
colnames  <- c("start", "end")
rownames  <- c("# [0-9]+","HOLE","FLOP","TURN","RIVER","SHOW DOWN","SUMMARY")       
numLegs   <- length(rownames)
legs      <- matrix(0, numLegs, length(colnames), dimnames = list(rownames, colnames))

# REGEXP Regular expression matrix for bet parsing init
rlabels <- c("bet", "win", "uncalled", "all-in")
clabels <- c("grep", "pattern", "player_ref", "action_ref", "bet_ref")

regexp <- matrix("", length(rlabels), length(clabels), dimnames = list(rlabels, clabels))

regexp["bet","grep"]    	<- "\\S+:.*\\d+\\S\\d+$"
regexp["bet","pattern"] 	<- "(^\\S[^:]+){1}(\\S\\s){1}(\\S+){1}.*\\$([0-9,.]+)$"
regexp["bet","player_ref"]	<- "\\1"
regexp["bet","action_ref"]	<- "\\3"
regexp["bet","bet_ref"]		<- "\\4"	
regexp["win","grep"]    	<- "collected"
regexp["win","pattern"] 	<- "(^.*)(\\s)(collected).*\\$([0-9,.]+).*"
regexp["win","player_ref"]	<- "\\1"
regexp["win","action_ref"]	<- "\\3"
regexp["win","bet_ref"]		<- "\\4"
regexp["uncalled","grep"]    	<- "^Uncalled bet"
regexp["uncalled","pattern"] 	<- "(^Uncalled bet).*\\$([0-9,.]+).*(returned to)\\s+(.*)"
regexp["uncalled","player_ref"]	<- "\\4"
regexp["uncalled","action_ref"]	<- "\\1"
regexp["uncalled","bet_ref"]	<- "\\2"
regexp["all-in","grep"]    	<- "all-in"
regexp["all-in","pattern"] 	<- "(^\\S[^:]+){1}(\\S\\s){1}(\\S+){1}.*\\$([0-9,.]+).*(all-in)$"
regexp["all-in","player_ref"]	<- "\\1"
regexp["all-in","action_ref"]	<- "\\5"
regexp["all-in","bet_ref"]	<- "\\4"

### MAIN parsing loop - cycle through hands
for (i in 1:numHands) {
    
    # Legs matrix - get start and end rows for each leg
    for(n in 1:numLegs) {
        pad <- "/*/*/*"
        pattern <- paste(pad, rownames(legs)[n], pad)
        start <- grep(pattern, hands[[i]])
        if(length(start) == 0)  {
            start <- 0
        }
        legs[n, "start"] <- start
    }

    legs[1:(numLegs-1), "end"] <- legs[2:numLegs,"start"]     # endrow= next start row
    legs[,"end"][legs[,"end"] == 0] <- legs[numLegs,"start"]  # if endrow=0 set to final start row
    legs[,"end"][legs[,"start"] == 0] <- 0                    # if startrow = 0 set end row to 0
    legs[numLegs, "end"] <- legs[numLegs, "start"]            # last endrow = last start row 
    
    # parse -  hand number & game type
    row <- hands[[i]][HANDNUM_ROW];
    pattern <- "(\\S+\\s+){2}(\\S)(\\d+:\\s+)(.*)"
    HandID[i] <-sub(pattern,"\\3",row)
    GameType[i] <-sub(pattern,"\\4",row)
    
    # parse - timestamp
    row <- hands[[i]][DATETIME_ROW];
    pattern <- "^(.*)(\\d{4}/\\d{1,2}/\\d{1,2} \\d{0,2}:\\d{0,2}:\\d{0,2}).*"
    Timestamp[i] <- sub(pattern, "\\2", row)

    # parse - table name
    row <- hands[[i]][TABLENAME_ROW];
    pattern <- "^(.*)('\\w+|'\\d+').*"
    TableName[i] <- sub(pattern, "\\2", row)

    # parse - player names
    pos <- grep("^Seat ",hands[[i]][1:legs["HOLE","start"]])
    row <- hands[[i]][pos]
    pattern <- "(\\S+\\s+){2}(\\w+).*"
    Players[i] <- list(sub(pattern, "\\2", row))

    # parse - hole cards
    row <- hands[[i]][legs["HOLE","start"]+1]
    pattern <- "(\\S+\\s+).*([0-9 A-Z]\\w){1}(\\s){1}([0-9 A-Z]\\w+).*"
    HoleCard1[i] <- sub(pattern, "\\2", row)   
    HoleCard2[i] <- sub(pattern, "\\4", row)
    
    # parse each leg
    for (n in 1:numLegs) {
        start_row <- legs[n,"start"]
        end_row   <- legs[n,"end"]
        leg_name  <- rownames(legs)[n]
	if(leg_name == "[0-9]+") {
		leg_name <- "BLIND"	
	}
        # If leg was played
        if(start > 0) {
            leg <- hands[[i]][start_row:end_row]
            # loop through regexp matrix
            for(m in 1:nrow(regexp)) {
                gp 		<- regexp[m,"grep"]
                pattern 	<- regexp[m,"pattern"]
		player_ref 	<- regexp[m,"player_ref"]
		action_ref	<- regexp[m,"action_ref"]
		bet_ref		<- regexp[m,"bet_ref"]                

                pos <- grep(gp,leg)
                row <- leg[pos]
             
                numMatches  <- length(sub(pattern, "\\1", row));
                
		#If match found - add row to BetMatrix
                if(numMatches > 0) {
                    hand_id     <- rep(HandID[i], numMatches);
                    leg_type    <- rep(leg_name, numMatches);
                    player      <- sub(pattern, player_ref, row);
                    action      <- sub(pattern, action_ref, row);
                    bet         <- sub(pattern, bet_ref, row);

                    end <- k + (numMatches-1)
                    BetMatrix[k:end,] <- matrix(c(hand_id, leg_type, player, action, bet),
                                                 numMatches,
                                                 ncol(BetMatrix))
                    k <- end+1
                }
            }
        }
    }      
}

# Cast timestamp as POSIX 
Timestamp <- gsub("/","-", Timestamp)
Timestamp <- strptime(Timestamp,"%Y-%m-%d %H:%M:%S")

# Add results to datatable
HandTable <- data.frame(HandID, Timestamp, TableName, GameType, HoleCard1, HoleCard2)

# Cast BetMatrix as data frame
BetMatrix <- na.omit(as.data.frame(BetMatrix))

# Cast BetMatrix$Bet column to numeric
BetMatrix$Bet 	 <- as.numeric(as.character(BetMatrix$Bet))

#Merge BetMatrix with HandTable
FullTable <- merge(HandTable, BetMatrix, by = "HandID")

############### WRITE TO OUTPUT  ########################################

#Write to csv
if (WRITE_PARSED_FLAG == TRUE) {
	print(paste("Writing output table to file", PARSED_FILE, "..."))
	write.csv(FullTable, file = PARSED_FILE)
	print("Write complete.")
}







