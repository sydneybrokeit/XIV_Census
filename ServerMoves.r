#Set these variables to the files you want to read
pastCsv <- "October.csv"
currentCsv <- "November.csv"
outputFile <- "ServerMoves.csv"
# End of user variables; once these are filled out, you can run the script

pastData <- read.csv(pastCsv, colClasses=c(NA, "NULL", NA, "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL"))
currentData <- read.csv(currentCsv, colClasses=c(NA, "NULL", NA, "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL"))

#Change Column Name for the current data's "realm" to "realmNow" for merging
colnames(currentData)[2] <- "realmNow"

#Merge by id field of data.frames; removes rows that don't exist in both
mergeSet <- merge(pastData, currentData, by = "id")

serverMoveTable <- table(mergeSet[c(2,3)])

proportionalSMTable <- prop.table(serverMoveTable, 1)


#removes the population still on the server from the table, using 0 so addmargins doesn't freak out
for(i in 1:64){
  serverMoveTable[i, i] <- 0
  proportionalSMTable[i, i] <- 0
}

serverMoveTable <- addmargins(serverMoveTable)

#removes the population still on the server from the table
for(i in 1:64){
  serverMoveTable[i, i] <- NA
  proportionalSMTable[i, i] <- NA
}

#Write serverMoveTable to .csv
write.csv(serverMoveTable, file=outputFile)
write.csv(proportionalSMTable, file="proportional.csv")
