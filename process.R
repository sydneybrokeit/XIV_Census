require(ggplot2); #for drawing
require(scales); #for adjusting scales in charts

#Set Filename Here
playerSetName <- "November.csv";
levelBreaks <- (0:12 * 5);
levelBreaks_small <- (0:6*10);

#Returns a string with the first letter of each capitalized
.simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1, 1)), substring(s, 2),
        sep = "", collapse = " ")
}

subByLevel <- function(dataSet, level){;
  return(subset(dataSet[apply(dataSet[, c(-1:-4)], MARGIN=1, function(x) {any(x >= level)}), ], realm != ""));
}

#Returns the class name from the format "level_$class"
getPlayerClassName <- function(class){;
  return(strsplit(class, "_")[[1]][2]);
}

# Vectors for creating jobClasses data.frame
jobsList <- c("Warrior", "Paladin", "Monk", "Dragoon", "Bard", "White Mage", "Black Mage", "Summoner", "Scholar", "Ninja", "Dark Knight", "Astrologian", "Machinist");
primList <- c("marauder", "gladiator", "pugilist", "lancer", "archer", "conjurer", "thaumaturge", "arcanist", "arcanist", "rogue", "darkknight","astrologian", "machinist");
secList <- c("gladiator", "conjurer", "lancer", "marauder", "pugilist", "arcanist", "archer", "thaumaturge", "conjurer", "pugilist", "gladiator", "conjurer", "archer");
terList <-  c("pugilist", "marauder", "marauder", "pugilist", "lancer", "thaumaturge", "arcanist", "archer", "thaumaturge", "lancer", "marauder", "thaumaturge", "lancer");

jobClasses <- data.frame(jobsList, primList, secList, terList, stringsAsFactors = FALSE);

#Sets None, Flames, Mael, and Adders colors
gc_colors <- c("#FFFFFF", "#333333","#FF6666","#DDDD00");


#Read playerSetName file (.csv file container data), omitting first two columns (ID and Name)
playerData <- read.csv(playerSetName, colClasses = c("NULL", "NULL", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA));
for(i in 5:27){
  colnames(playerData)[i] <- getPlayerClassName(colnames(playerData)[i])
  }
#Get list of classes
playerClasses <- names(playerData)[5:27];

#Get list of servers
serverNames <- levels(playerData[[1]]);

generateCharts <- function(pDataSet, location, appendText) {
  dir.create(location);
  dir.create(paste(location, "/faceted", sep=""));
  #Generates per-class charts of levels
  for(i in playerClasses) {;
    pDataSubset <- pDataSet[, c("realm", i)];
    pcName <- .simpleCap(i);
    saveLocation <- paste(location,"/",pcName,".png", sep="");
    print(ggplot(pDataSubset, aes_string(x=i)) + geom_histogram(binwidth = 1, origin=0.5) + scale_x_continuous(breaks=levelBreaks, name=paste(pcName, appendText)) + scale_y_continuous(labels=comma));
    ggsave(saveLocation, width=11.2, height=6.06);
    saveLocation <- paste(location,"/faceted/",pcName,".png", sep="");
   print(ggplot(pDataSubset, aes_string(x=i)) + geom_histogram(binwidth = 1, origin=0.5) + scale_x_continuous(breaks=levelBreaks_small, name=paste(pcName, appendText)) + facet_wrap(~realm) + theme(axis.text.x=element_text(angle=-90, vjust=0.5)) );
    ggsave(saveLocation, height=11, width=11, units="in");
  }
  #Generates Grand Company charts
  grandCompanies <- subset(pDataSet[4], grand_company != "");
  ggplot(grandCompanies, aes(x=grand_company)) + geom_bar(stat="bin") + scale_x_discrete(name=paste("Grand Company", appendText)) + stat_bin(geom = "text", aes(label = paste((..count..)), vjust = 5));
  ggsave(paste(location, "/GC.png", sep=""));
  
  dir.create(paste(location, "/demographics/", sep=""));
  #Creates Ranked charts for demographics
  popCount <- table(pDataSet[1]);
  serverRanking <- order(popCount, decreasing = TRUE);
  popCount <- popCount[serverRanking];
  ggplot(pDataSet, aes(x=realm, fill=gender)) + geom_bar(stat="bin") + scale_x_discrete(name=paste("Server Population (with gender)",appendText), limits=names(popCount)) + theme(axis.text.x=element_text(angle=-90, vjust=0.5)) + scale_y_continuous(labels = comma);
  ggsave(paste(location, "/demographics/serverPopGender.png", sep=""), width=11.2, height=6.06);
  ggplot(pDataSet, aes(x=realm, fill=grand_company)) + geom_bar(stat="bin") + scale_x_discrete(name=paste("GC Affiliation by Server", appendText), limits=names(popCount)) + theme(axis.text.x=element_text(angle=-90, vjust=0.5)) + scale_y_continuous(labels = comma) + scale_fill_manual(values=gc_colors);
  ggsave(paste(location, "/demographics/serverPopGC.png", sep=""), width=11.2, height=6.06);
  ggplot(pDataSet, aes(x=race, fill=gender)) + geom_bar(position="dodge") + scale_x_discrete(name=paste("Races", appendText))  + stat_bin(geom = "text", aes(label = paste((..count..)), vjust = -0.25), position=position_dodge(width=0.9)) + stat_bin(geom = "text", aes(label = paste(round((..count..)/sum(..count..)*100, 2), "%", sep=""), vjust = 2), position=position_dodge(width=0.9));
  ggsave(paste(location, "/demographics/race.png", sep=""), width=11.2, height=6.06);
  ggplot(pDataSet, aes(x=realm, fill=race)) + geom_bar(stat="bin") + scale_x_discrete(name=paste("Server Population (by race)",appendText), limits=names(popCount)) + theme(axis.text.x=element_text(angle=-90, vjust=0.5)) + scale_y_continuous(labels = comma);
  ggsave(paste(location, "/demographics/serverpopRace.png", sep=""), width=11.2, height=6.06);
}

generateCharts(playerData, "general", "(Overall)");

playerData_50 <- subByLevel(playerData, 50);
rm(playerData);
generateCharts(playerData_50, "lv50", "(50+)");

playerData_60 <- subByLevel(playerData_50, 60);
generateCharts(playerData_60, "lv60", "(60+)");

#To allow importing level 60 data into other processes
save(playerData_60, file="playerData_60.rdata")

dir.create("lv60/crossclass");


for(i in 1:length(jobClasses[[1]])){;
  jobs <- jobClasses[[1]][i];
  primClass <- jobClasses[[2]][i];
  secClass <- jobClasses[[3]][i];
  terClass <- jobClasses[[4]][i];
  WorkingSet <- subset(playerData_60, playerData_60[primClass] == 60);
  location <- "lv60/crossclass";
  saveLocation <- paste(location,"/",jobs,".png", sep="");
  ggplot(WorkingSet, aes()) + geom_bar(aes_string(x=secClass, fill = shQuote("Secondary")), color="red", alpha=0.5, binwidth=1, origin=.5) + geom_bar(aes_string(x=terClass, fill=shQuote("Tertiary")), color="#0000AA", alpha=.5, binwidth=1, origin=.5) + scale_x_discrete(breaks=levelBreaks, name=paste("Cross-Classes for", jobs)) + scale_fill_manual(name="Cross-Class", values=c("Secondary"="#AA0000", "Tertiary" = "#0000AA"), labels=c(.simpleCap(toString(secClass)), .simpleCap(toString(terClass))));
  ggsave(saveLocation, width=11.2, height=6.06)
}  