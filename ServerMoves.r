pastData <- read.csv("October.csv", colClasses=c(NA, "NULL", NA, "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL"))
currentData <- read.csv("November.csv", colClasses=c(NA, "NULL", NA, "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL"))

#Change Column Name for the current data's "realm" to "realmNow" for merging
colnames(currentData)[2] <- "realmNow"

#Merge by id field of data.frames; removes rows that don't exist in both
mergeSet <- merge(pastData, currentData, by = "id")

serverMoveTable <- table(mergeSet[c(2,3)])

#removes the population still on the server from the table
for(i in 1:64){
  serverMoveTable[i, i] <- 0
}

serverMoveTable <- addmargins(serverMoveTable)

#removes the population still on the server from the table
for(i in 1:64){
  serverMoveTable[i, i] <- NA
}

#Write serverMoveTable to .csv
write.csv(serverMoveTable, file="ServerMoves.csv")