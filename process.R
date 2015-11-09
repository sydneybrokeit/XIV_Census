require(ggplot2); #for drawing
require(scales); #for adjusting scales in charts

#Set Filename Here
playerSetName <- "FullSet.csv";
levelBreaks <- (0:12 * 5);
levelBreaks_small <- (0:6*10);

#Returns a string with the first letter of each capitalized
.simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1, 1)), substring(s, 2),
        sep = "", collapse = " ")
}

subByLevel <- function(dataSet, level){;
  return(subset(playerData[apply(dataSet[, c(-1:-4)], MARGIN=1, function(x) {any(x >= level)}), ], realm != ""));
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

#Create folders
dir.create("general");
dir.create("general/faceted");

#Read playerSetName file (.csv file container data), omitting first two columns (ID and Name)
playerData <- read.csv("FullSet.csv", colClasses = c("NULL", "NULL", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA));
for(i in 5:27){
  colnames(playerData)[i] <- getPlayerClassName(colnames(playerData)[i])
  }
#Get list of classes
playerClasses <- names(playerData)[5:27];

#Get list of servers
serverNames <- levels(playerData[[1]]);


#Generates per-class charts of levels
for(i in playerClasses) {;
  location <- "general";
  pcName <- .simpleCap(i);
  saveLocation <- paste(location,"/",pcName,".png", sep="");
  print(ggplot(playerData, aes_string(x=i)) + geom_histogram(binwidth=5, origin=0.5) + geom_histogram(binwidth = 1, color="white", fill="white", origin=0.5) + scale_x_continuous(breaks=levelBreaks, name=pcName) + scale_y_continuous(labels=comma));
  ggsave(saveLocation);
}

#generates per-class,server charts of levels
for(i in playerClasses) {
  location <- "general/faceted";
  pcName <- .simpleCap(i);
  saveLocation <- paste(location,"/",pcName,".png", sep="");
  print(ggplot(playerData, aes_string(x=i)) + geom_histogram(binwidth = 1, origin=0.5) + scale_x_continuous(breaks=levelBreaks_small, name=pcName) + facet_wrap(~realm) + theme(axis.text.x=element_text(angle=-90, vjust=0.5)) );
  ggsave(saveLocation, height=11, width=11, units="in");
}

#Create GC plot (general)
grandCompanies <- subset(playerData[4], grand_company != "");
ggplot(grandCompanies, aes(x=grand_company)) + geom_bar(stat="bin") + scale_x_discrete(name="Grand Company");
ggsave("general/GC.png");

popCount <- table(playerData[1]);
serverRanking <- order(popCount, decreasing = TRUE);
popCount <- popCount[serverRanking];
ggplot(playerData, aes(x=realm, fill=gender)) + geom_bar(stat="bin") + scale_x_discrete(name="Server Population (with gender)", limits=names(popCount)) + theme(axis.text.x=element_text(angle=-90, vjust=0.5)) + scale_y_continuous(labels = comma);
ggsave("general/serverPopGender.png")
ggplot(playerData, aes(x=realm, fill=grand_company)) + geom_bar(stat="bin") + scale_x_discrete(name="GC Affiliation by Server", limits=names(popCount)) + theme(axis.text.x=element_text(angle=-90, vjust=0.5)) + scale_y_continuous(labels = comma) + scale_fill_manual(values=gc_colors)
ggsave("general/serverPopGC.png");
ggplot(playerData, aes(x=race, fill=gender)) + geom_bar(position="dodge") + scale_x_discrete(name="Races (General Population)");
ggsave("general/race.png");

dir.create("general/lv50");
playerData_50 <- subByLevel(playerData, 50);
rm(playerData);


for(i in playerClasses) {;
  location <- "general/lv50";
  pcName <- .simpleCap(i);
  saveLocation <- paste(location,"/",pcName,".png", sep="");
  print(ggplot(playerData_50, aes_string(x=i)) + geom_histogram(binwidth = 1, origin=0.5) + scale_x_continuous(breaks=levelBreaks, name=paste(pcName,"(Lv 50 Characters)")) + scale_y_continuous(labels=comma));
  ggsave(saveLocation, width=11.2, height=6.06);
}

popCount <- table(playerData_50[1]);
serverRanking <- order(popCount, decreasing = TRUE);
popCount <- popCount[serverRanking];
ggplot(playerData_50, aes(x=realm, fill=gender)) + geom_bar(stat="bin") + scale_x_discrete(name="Server Population (50+ Characters)", limits=names(popCount)) + theme(axis.text.x=element_text(angle=-90, vjust=0.5)) + scale_y_continuous(labels = comma);
ggsave("general/lv50/serverPopGender.png", width=11.2, height=6.06)
ggplot(playerData_50, aes(x=realm, fill=grand_company)) + geom_bar(stat="bin") + scale_x_discrete(name="GC Affiliation by Server (50+ Characters", limits=names(popCount)) + theme(axis.text.x=element_text(angle=-90, vjust=0.5)) + scale_y_continuous(labels = comma) + scale_fill_manual(values=gc_colors)
ggsave("general/lv50/serverPopGC.png", width=11.2, height=6.06);
ggplot(playerData_50, aes(x=race, fill=gender)) + geom_bar(position="dodge") + scale_x_discrete(name="Races, (50+ Characters)") + scale_y_continuous(labels=comma);
ggsave("general/lv50/race.png", width=11.2, height=6.06);

dir.create("general/lv60");
playerData_60 <- subByLevel(playerData_50, 60);

for(i in playerClasses) {;
  location <- "general/lv60";
  pcName <- .simpleCap(i);
  saveLocation <- paste(location,"/",pcName,".png", sep="");
  print(ggplot(playerData_60, aes_string(x=i)) + geom_histogram(binwidth = 1, origin=0.5) + scale_x_continuous(breaks=levelBreaks, name=paste(pcName,"(Lv 60 Characters)")) + scale_y_continuous(labels=comma));
  ggsave(saveLocation);
}

popCount <- table(playerData_60[1]);
serverRanking <- order(popCount, decreasing = TRUE);
popCount <- popCount[serverRanking];
ggplot(playerData_60, aes(x=realm, fill=gender)) + geom_bar(stat="bin") + scale_x_discrete(name="Server Population (60+ Characters)", limits=names(popCount)) + theme(axis.text.x=element_text(angle=-90, vjust=0.5)) + scale_y_continuous(labels = comma);
ggsave("general/lv60/serverPopGender.png", width=11.2, height=6.06 );
ggplot(playerData_60, aes(x=realm, fill=grand_company)) + geom_bar(stat="bin") + scale_x_discrete(name="GC Affiliation by Server (60+ Characters", limits=names(popCount)) + theme(axis.text.x=element_text(angle=-90, vjust=0.5)) + scale_y_continuous(labels = comma) + scale_fill_manual(values=gc_colors);
ggsave("general/lv60/serverPopGC.png", width=11.2, height=6.06);
ggplot(playerData_60, aes(x=race, fill=gender)) + geom_bar(position="dodge") + scale_x_discrete(name="Races, (60+ Characters)");
ggsave("general/lv60/race.png", width=11.2, height=6.06);


dir.create("general/lv60/crossclass");


for(i in 1:length(jobClass[[1]])){;
  jobs <- jobClasses[[1]][i];
  primClass <- jobClasses[[2]][i];
  secClass <- jobClasses[[3]][i];
  terClass <- jobClasses[[4]][i];
  WorkingSet <- subset(playerData_60, playerData_60[primClass] == 60);
  location <- "general/lv60/crossclass";
  saveLocation <- paste(location,"/",jobs,".png", sep="");
  ggplot(WorkingSet, aes()) + geom_bar(aes_string(x=secClass, fill = shQuote("Secondary")), color="red", alpha=0.5, binwidth=1, origin=.5) + geom_bar(aes_string(x=terClass, fill=shQuote("Tertiary")), color="#0000AA", alpha=.5, binwidth=1, origin=.5) + scale_x_discrete(breaks=levelBreaks, name=paste("Cross-Classes for", jobs)) + scale_fill_manual(name="Cross-Class", values=c("Secondary"="#AA0000", "Tertiary" = "#0000AA"), labels=c(.simpleCap(toString(secClass)), .simpleCap(toString(terClass))));
  ggsave(saveLocation, width=11.2, height=6.06)
}                                                                                                                                                                                                                                                                                                        