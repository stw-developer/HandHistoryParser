
#################################################################
# hh_analysis.r - poker hand history analyser
# hand history analyzer for PokerStars
# analyzes handhistory text file from pokerstars and
# saves to csv. Also outputs Running profit balance, profit/hand
# running profit graphs.
#
# Calculates the following:
# - Hand statistics (hands (dealt, played, won) in absolute and % terms)
# - Performance (bb/100, VFIP, PFR)
# - Profitability (net profit, average profit, 100-hand average) 
#
# Author: sherhan wicky
# Date  : 30/04/2017
# Ver   : 0.1

############### LIBRARIES    #########################################
source("hh_parser_config.r")
source("hh_parser.r")

############### GLOBAL VARS ##########################################
# set numeric precision to 2 decimal places	
options(digits=2)

ActionTypes 	<- sort(as.character(unique(FullTable$Action)))
numActionTypes	<- length(ActionTypes)

############### FUNCTIONS  #############################################

# create an hourly spaced timeline between two dates
make_hrly_timeline <- function(start_dt, end_dt) {
    # round down to the closest hour
    earliest <- round(start_dt, units="hours")
    
    # number of hours between start and end times
    time_diff   <- as.numeric(round(difftime(end_dt,start_dt, units="hours")))

    # hourly increments expressed in seconds
    hr_inc <- seq(0,3600*time_diff, 3600)
    #print(hr_inc)

    # return hourly timeline
    tl <- earliest + hr_inc 
}

# calculates profit as difference between collected+uncalled amounts (earnings)
# less all other bet types (costs)
# 
# i.e. profit = (collected + uncalled) - (bets + calls + raises + all_in)
#
calc_profit <- function(bet.all_in, bet.bets, bet.calls, bet.collected, 
			bet.posted, bet.raises, bet.uncalled) {
	
	earnings <- bet.collected + bet.uncalled	
	cost	 <- bet.bets + bet.calls + bet.raises + bet.posted + bet.all_in
	
	profit 	 <- earnings - cost 
}

#calculate the running sum for a vector x over a window with n elements
rsum <- function(x, n) {
	cx   	    <- cumsum(x)
	running_sum <- (cx[(n+1):length(x)] - cx[1:(length(x) - n)]) #/ n

}

#calculate the window mean: average of the sum of windows with n elements
wmean <- function(x,n) {
	len	<- length(x)
	mx	<- cumsum(x)
	start   <- seq(len,1, -n)
	end	<- c(start[-1], 1)

	window_mean <- sum(mx[start] - mx[end])/length(start)
}

########### MAIN #########################################################################

#Extract big blinds per hand column 
cols   <- c("HandID", "Bet")
bbCol  <- subset(FullTable, Action == "posts", select = cols)
bbCol  <- aggregate(bbCol$Bet, by = list(bbCol$HandID), max)
colnames(bbCol) <- c("HandID", "BigBlind")

#Extract bets per player
cols <- c("Timestamp", "HandID", "TableName", "Action", "Bet")
pbets <- subset(FullTable, Player == PLAYER_NAME, select = cols)

#Add big blind column to player bets dataframe
pbets <- merge(x=pbets, y=bbCol, by = "HandID", all.x=TRUE)


#### PER HAND ANALYSIS ###########


#Get unique HandIDs as dataframe
uniqueHand		<- subset(pbets, select=c("HandID"))
uniqueHand		<- unique(uniqueHand)
numHand 	    	<- length(uniqueHand)

#Initialise Bet Table - pivoted table
BetTbl  <- uniqueHand

#Aggregate bet totals by action
h_total <- aggregate(.~Timestamp+HandID+TableName+Action, data=pbets, sum) 

#Add timestamp column to BetTable
tsCol 		<- aggregate(pbets$Timestamp, by = list(pbets$HandID), max)
colnames(tsCol) <- c("HandID", "Timestamp")
BetTbl 		<- cbind(BetTbl, tsCol[2])

#Add big blind column to BetTable
bbCol 		<- aggregate(pbets$BigBlind, by = list(pbets$HandID), max)
colnames(bbCol) <- c("HandID", "BigBlind")
BetTbl 		<- cbind(BetTbl, bbCol[2])

#Pivot pbets dataframe: pivot actions column
for (i in 1:numActionTypes) {
	# extract column to pivot	
	act <- ActionTypes[i] 
	betCol <- subset(h_total, Action == act, select=c("HandID", "Bet"))
	 colnames(betCol) <- c("HandID", act)
	betCol <- merge(x=uniqueHand, y=betCol, by = "HandID", all.x = TRUE)
	
	# add column to output table
	BetTbl <- cbind(BetTbl, betCol[2])
}

#Replace NAs with zeros
BetTbl[is.na(BetTbl)] <- 0

#Calculate profit per hand
BetTbl["profit"] <- calc_profit(BetTbl["all-in"],
				BetTbl["bets"],
				BetTbl["calls"],
				BetTbl["collected"],
				BetTbl["posts"],
				BetTbl["raises"],
				BetTbl["Uncalled bet"])

#Calculate balance (running sum with window = 1)
BetTbl["balance"] <- cumsum(BetTbl["profit"])


#Calculate profit in Big Blind terms
BetTbl["profit_bb"] <- round(BetTbl["profit"]/BetTbl["BigBlind"],1)

#Calculate balance (running sum with window = 1)
BetTbl["balance_bb"] <- cumsum(BetTbl["profit_bb"])


#Calculate running sums with window = WINDOW_SIZE
profit 	   <- BetTbl$profit
profit_bb  <- BetTbl$profit_bb
numProfits <- length(profit)

if(numProfits > WINDOW_SIZE) {
	running_profit 	  <- rsum(profit, WINDOW_SIZE)
	running_profit_bb <- rsum(profit_bb, WINDOW_SIZE)	

	pad_zeros   <- rep(0, numProfits-length(running_profit)) 
	BetTbl["running_sum"] 	 <- c(pad_zeros, running_profit)
	BetTbl["running_sum_bb"] <- c(pad_zeros, running_profit_bb)
} else {
	pad_zeros   <- rep(0, numProfits) 
	BetTbl["running_sum"] 	 <- pad_zeros
	BetTbl["running_sum_bb"] <- pad_zeros
}	


#### PER HOUR ANALYSIS ####################

# Add hourly column to playerTbl
Hour  <- cut(pbets$Timestamp, breaks="hour")
pbets <- data.frame(pbets, Hour)	

# split table into hourly blocks
blocks <- split(pbets, pbets$Hour)

# remove empty blocks
blocks <- blocks[lapply(blocks	,nrow)>0]

#initialise per hour vars
numBlocks     	<- length(blocks)

#initialise bet totals by action
bet_totals	<- matrix(0, numBlocks, numActionTypes)
colnames(bet_totals) <- ActionTypes

# calculation per hourly block
for(i in 1:numBlocks) {
  totals  <- aggregate(. ~ Action, data=blocks[[i]], sum)
  actions <- as.character(totals[,"Action"])
  bet_totals[i,actions] <- totals[, "Bet"] 	
}

#get max/min timestamps
#ts 	 <- playerTbl$Timestamp
#dt_start <- min(ts)
#dt_end   <- max(ts)

# create hourly time buckets
#buckets  <- make_hrly_timeline(dt_start, dt_end)
#nBuckets <- length(buckets)

# dev stuff -  need to vectorise this
#res 	 <- ts[ts < buckets[10] & ts > buckets[9]]

#pos	<- grep("[^Tournament]", FullTable$GameType)	# filter out tournaments
#myBets 	<- FullTable[pos,c("Action","Bet")]

#### CALCS FOR OUTPUT ########################

#CALCTABLE init
run_profit_label     <- paste("Latest  ",WINDOW_SIZE,"-hand Profit  ", sep="")
run_profit_label_ave <- paste("Average ",WINDOW_SIZE,"-hand Profit  ", sep="")
colnames  	     <- c("$", "bb")
rownames  	     <- c("Net Profit", "Average Profit/hand", run_profit_label, run_profit_label_ave)       
CalcTable      	     <- matrix(0, length(rownames), length(colnames), dim=list(rownames,colnames))

#Calc profits and store
CalcTable["Net Profit", "$"] 	  	<- BetTbl$balance[nrow(BetTbl)] 
CalcTable["Net Profit", "bb"] 	  	<- BetTbl$balance_bb[nrow(BetTbl)]
CalcTable["Average Profit/hand", "$"]  	<- BetTbl$balance[nrow(BetTbl)] /numHands 
CalcTable["Average Profit/hand", "bb"] 	<- BetTbl$balance_bb[nrow(BetTbl)]/numHands
CalcTable[run_profit_label, "$"]  	<- BetTbl$running_sum[nrow(BetTbl)] 
CalcTable[run_profit_label, "bb"]    	<- BetTbl$running_sum_bb[nrow(BetTbl)]
CalcTable[run_profit_label_ave, "$"]	<- wmean(BetTbl$profit, WINDOW_SIZE) 
CalcTable[run_profit_label_ave, "bb"] 	<- wmean(BetTbl$profit_bb, WINDOW_SIZE)  
CalcTable 			 	<- round(CalcTable,2)

#Calc hand numbers
handsDealt   	<- numHands
handsPlayed 	<- numProfits
handsPlayed_pct <- round(handsPlayed/handsDealt*100)
handsWon    	<- length(which(BetTbl$profit > 0))
handsWon_pct	<- round(handsWon/handsPlayed*100)

#Calc bb/100 (big blinds per 100 hands). 
# This stat shows the profit in terms of big blinds per 100 hands.
bb_100		<- round(CalcTable["Net Profit", "bb"]/(handsDealt/100),1)


#Calc PFR (pre-flop raise). Percentage of time pre-flop raised
#
#Calc VPIP (VPIP Voluntarily Put $ In Pot)
# This stat shows the percentage of time you 
# make calls or raises before the flop (hence "putting money in to the pot").
# Note: The small blind and big blind do not count toward your VPIP, as you did not 
# voluntarily put this money in to the pot (hence the stat's name). 
# So if you end up seeing a flop after checking on the BB, it will have no effect on 
# your VPIP.
pre_flop_calls	   <- subset(FullTable, Player=="the_wickstar" & 
					LegType=="HOLE" & 
					Action=="calls", select=c(Bet))
pre_flop_raises	   <- subset(FullTable, Player=="the_wickstar" & 
					LegType=="HOLE" & 
					Action=="raises", select=c(Bet))
num_pfc		   <- nrow(pre_flop_calls)
num_pfr		   <- nrow(pre_flop_raises)

vpip		   <- round((num_pfc + num_pfr)/handsDealt*100)		
pfr		   <- round((num_pfr)/handsDealt*100)

#Print calcs to std out.
cat("\n\n")
cat("#### HAND STATS  ######")
cat("\n\n")
cat(paste("Hands dealt  =", handsDealt,"\n"))
cat(paste("Hands played =", handsPlayed,"\n"))
cat(paste("Hands won    =", handsWon,"\n"))
cat("\n")
cat(paste("% Hands played =", handsPlayed_pct ,"%","\n"))
cat(paste("% Hands won    =", handsWon_pct,"%","\n"))
cat("\n")
cat("#### PERFORMANCE ######")
cat("\n\n")
cat(paste("bb/100 =", bb_100,"\n"))
cat(paste("VPIP   =", vpip ,"%","\n"))
cat(paste("PFR    =", pfr ,"%","\n"))
cat("\n")
cat("#### PROFIT TABLE #####")
cat("\n")
print(CalcTable)
cat("\n\n")

#### PLOTS ###################################


#create 4x1 placeholder for plots
par(mfrow=c(4,1))

#running profit - $
plot(BetTbl$balance, type="h", col = "slate blue", panel.first=grid(),
		main = "Running Profit ($)", 
		xlab = "Number of Hands", 
		ylab = "Profit")



#profit per hand played - $
plot(BetTbl$profit, type="l", col = "dark green", panel.first=grid(), 
		main = "Profit per hand ($)", 
		xlab = "Hand", 
		ylab = "Profit")

#running 100 hand profit - $
plot(running_profit, type="h", col = "dark red", panel.first=grid(),
		main = paste("Running ",WINDOW_SIZE,"-hand Profit ($)", sep=""), 
		xlab = "Number of Hands", 
		ylab = "Profit")

#running 100 hand profit - bb
plot(running_profit_bb, type="h", col = "dark orange", panel.first=grid(),
		main = paste("Running ",WINDOW_SIZE,"-hand Profit (bb's)", sep=""), 
		xlab = "Number of Hands", 
		ylab = "Number of Big Blinds won/lost")


############### WRITE TO OUTPUT  ###############

#Write to csv
if (WRITE_ANALYSIS_FLAG == TRUE) {
	print(paste("Writing analysis table to file", ANALYSIS_FILE, "..."))
	write.csv(BetTbl, file = ANALYSIS_FILE)
	print("Write complete.")
}





