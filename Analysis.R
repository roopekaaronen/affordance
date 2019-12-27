################################################################
##    Roope Kaaronen   // University of Helsinki // IIASA
##    Date: 26.12.2019
##    Contact: roope dot kaaronen at helsinki dot fi
##    https://roopekaaronen.com
##    @roopekaaronen
##    GitHub: https://github.com/roopekaaronen
##    ROOPE KAARONEN, 2019
################################################################

################################################################
##    NOTES:
##    This R code includes data-analysis for the NetLogo model
##    AFFORDANCE LANDSCAPE MODEL: CULTURAL EVOLUTION OF 
##    PRO-ENVIRONMENTAL BEHAVIORS
##    The model is available at CoMSES: 
##    https://www.comses.net/codebases/c2feceb8-d9c4-4637-8f27-fda49c7dc4f3/releases/1.2.0/
################################################################

################################################################
##    Load packages (install first, if necessary)
################################################################
setwd("C:/Users/RoopeOK/Documents/Yliopisto/Affordance model/affordance")

library(tidyverse)
library(nlrx)
library(factoextra)
library(Hmisc)
library(plyr)
library(RColorBrewer)
library(reshape2)
library(ggpubr)
library(gridExtra)


################################################################
##    START OF ABSTRACT MODEL ANALYSIS
################################################################

##################################
##    BATCH OF 3000 RUNS
##################################

# READ DATA
dataB = read.csv("https://raw.githubusercontent.com/roopekaaronen/affordance/master/Stylized.batch.csv", skip = 6, stringsAsFactors = FALSE)


# TRANSFORM TOTAL NUMBER OF BEHAVIOR INTO PROPORTION
dataB$pro.behavior <- dataB$pro.behavior/dataB$number.of.agents
dataB$non.behavior <- dataB$non.behavior/dataB$number.of.agents

# CREATE TWO DATASETS FROM DATA: ONE WITH NICHE CONSTRUCTION AND ONE WITHOUT NICHE CONSTRUCTION

# WITH NICHE CONSTRUCTION
withniche <- dataB[ which(dataB$niche.construction=="true"), 1:24]

# WITHOUT NICHE CONSTRUCTION
noniche <- dataB[ which(dataB$niche.construction=="false"), 1:24]

## CREATE DATA-FRAME FOR NICHE CONSTRUCTION DATA
datB <- data.frame(withniche) 

## GROUP (AGGREGATE) DATA BY PRO-AMOUNT
aggB <- datB %>%
  group_by(pro.amount)

theme_update(text = element_text(size=6.5), legend.position="bottom")

## PLOT AMOUNT OF PRO-ENVIRONMENTAL AFFORDANCES BY PRO-ENVIRONMENTAL BEHAVIORS (WITH SMOOTHED CONDITIONAL MEANS)
pB <- ggplot(data = aggB) +
  geom_point(aes(pro.amount, pro.behavior, colour="Proenvironmental"), alpha = 0.1, size = 0.3) +
  geom_smooth(aes(pro.amount, pro.behavior), se = FALSE, method = "loess", show.legend = FALSE, colour = "gray20", size = 0.5, span=0.15) +
  geom_point(aes(pro.amount, non.behavior, colour="Nonenvironmental"), alpha = 0.1, size = 0.3) + 
  geom_smooth(aes(pro.amount, non.behavior), se = F, method = "loess", show.legend = FALSE, colour = "firebrick3", size = 0.5, span=0.15) +
  xlab("Proportion of pro-environmental affordance") +
  ylab("Proportion of environmentally significant behaviors") +
  ggtitle("Environmental behavior as a function of affordances", subtitle = "3030 runs with niche construction") +
  scale_colour_manual(name="Behavior",
                      values=c(Proenvironmental="gray20", Nonenvironmental="firebrick3")) +
  theme(legend.position="bottom") +
  guides(colour = guide_legend(override.aes = list(size=2)))
## PRINT PLOT
pB


## REPEAT PROCEDURES ABOVE FOR DATA WITH NO NICHE CONSTRUCTION
## CREATE DATA-FRAME
datB2 <- data.frame(noniche) 

## GROUP (AGGREGATE) DATA BY PRO-AMOUNT
aggB2 <- datB2 %>%
  group_by(pro.amount)

## PLOT AMOUNT OF PRO-ENVIRONMENTAL AFFORDANCES BY PRO-ENVIRONMENTAL BEHAVIORS (WITH SMOOTHED CONDITIONAL MEANS)

pB2 <- ggplot(data = aggB2) +
  geom_point(aes(pro.amount, pro.behavior, colour="Proenvironmental"), alpha = 0.1, size = 0.3) +
  geom_smooth(aes(pro.amount, pro.behavior), se = FALSE, method = "loess", show.legend = FALSE, colour = "gray20", size = 0.5, span=0.15) +
  geom_point(aes(pro.amount, non.behavior, colour="Nonenvironmental"), alpha = 0.1, size = 0.3) + 
  geom_smooth(aes(pro.amount, non.behavior), se = F, method = "loess", show.legend = FALSE, colour = "firebrick3", size = 0.5, span=0.15) +
  xlab("Proportion of pro-environmental affordance") +
  ylab("Proportion of environmentally significant behaviors") +
  ggtitle("Environmental behavior as a function of affordances", subtitle = "3030 runs without niche construction") +
  scale_colour_manual(name="Behavior",
                      values=c(Proenvironmental="gray20", Nonenvironmental="firebrick3")) +
  guides(colour = guide_legend(override.aes = list(size=2)))
## PRINT PLOT
pB2

##################################
##    TIME SERIES of ABSTRACT MODEL
##################################

## DOWNLOAD DATA FROM https://github.com/roopekaaronen/affordance/blob/master/Stylized.timeseries.csv.zip?raw=true
## READ DATA
dataT = read.csv("C:/Users/RoopeOK/Documents/Yliopisto/Affordance model/Analysis/CSV/Stylized.timeseries.csv", stringsAsFactors = FALSE)

# CREATE SUBSETS OF DATA

# NEUTRAL (50% PRO-AFFORDANCE) WITH NICHE CONSTRUCTION
neutral <- dataT[ which(dataT$pro.amount==0.5 & dataT$niche.construction=="true"), 1:7]
neutral$X.run.number. <- as.factor(neutral$X.run.number.)

# NEUTRAL (50% PRO-AFFORDANCE) WITHOUT NICHE CONSTRUCTION
neutral2 <- dataT[ which(dataT$pro.amount==0.5 & dataT$niche.construction=="false"), 1:7]
neutral2$X.run.number. <- as.factor(neutral$X.run.number.)

# BIASED LANDSCAPE (60% PRO-AFFORDANCE) WITH NICHE CONSTRUCTION
bias <- dataT[ which(dataT$pro.amount==0.6 & dataT$niche.construction=="true"), 1:7]
bias$X.run.number. <- as.factor(bias$X.run.number.)

# BIASED LANDSCAPE (60% PRO-AFFORDANCE) WITHOUT NICHE CONSTRUCTION
bias2 <- dataT[ which(dataT$pro.amount==0.6 & dataT$niche.construction=="false"), 1:7]
bias2$X.run.number. <- as.factor(bias2$X.run.number.)

## CREATE SUMMARY DATA FOR NEUTRAL SUBSETS
means.neutral <- neutral %>% 
  group_by(X.step.) %>%
  dplyr::summarize(meanpro = mean(pro.behavior), meannon = mean(non.behavior), sdpro = sd(pro.behavior), sdnon = sd(non.behavior))

means.neutral2 <- neutral2 %>% 
  group_by(X.step.) %>%
  dplyr::summarize(meanpro = mean(pro.behavior), meannon = mean(non.behavior), sdpro = sd(pro.behavior), sdnon = sd(non.behavior))

## REMOVE FIRST ROW (BLANK)
means.neutral <- means.neutral[-c(1),]
means.neutral2 <- means.neutral2[-c(1),]

## CREATE SUMMARY DATA FOR BIASED SUBSETS
means.bias <- bias %>% 
  group_by(X.step.) %>%
  dplyr::summarize(meanpro = mean(pro.behavior), meannon = mean(non.behavior), 
                   sdpro = sd(pro.behavior), sdnon = sd(non.behavior))

means.bias2 <- bias2 %>% 
  group_by(X.step.) %>%
  dplyr::summarize(meanpro = mean(pro.behavior), meannon = mean(non.behavior), 
                   sdpro = sd(pro.behavior), sdnon = sd(non.behavior))

## REMOVE FIRST ROW (BLANK)
means.bias <- means.bias[-c(1),]
means.bias2 <- means.bias2[-c(1),]


## PLOT TIME-SERIES DATA OF THE DEVELOPMENT OF PRO-BEHAVIOR OVER TIME

# PLOT OF MEAN PRO-ENVIRONMENTAL BEHAVIOR OVER TIME FOR 50% AFFORDANCE (WITH NICHE CONSTRUCTION)

pn <- ggplot(data=means.neutral) + 
  geom_ribbon(aes(ymin=meanpro-sdpro, ymax=meanpro+sdpro, x=X.step., fill = "band"), alpha = 0.2, fill = "gray20")+
  geom_smooth(aes(x=X.step., y=meanpro), color = "gray20", se = FALSE, size = 0.5) +
#  geom_smooth(data=neutral, aes(group=X.run.number., x=X.step., y=pro.behavior), se = FALSE, color = "gray20") + # Remove hashtag to view all individual lines.
  xlab("Step") +
  ylab("Proportion of pro-environmental behaviors") +
  ggtitle("Time series of pro-environmental behavior", subtitle = 
           "50% pro-environmental affordances\n300 runs with niche construction") +
  coord_cartesian(ylim = c(0, 1), xlim =c(0,2000)) 

# Print plot
pn

# PLOT OF MEAN PRO-ENVIRONMENTAL BEHAVIOR OVER TIME FOR 50% AFFORDANCE (WITHOUT NICHE CONSTRUCTION)

pn2 <- ggplot(data=means.neutral2) + 
  geom_ribbon(aes(ymin=meanpro-sdpro, ymax=meanpro+sdpro, x=X.step., fill = "band"), alpha = 0.2, fill = "gray20")+
  geom_smooth(aes(x=X.step., y=meanpro), color = "gray20", se = FALSE, size = 0.5) +
#   geom_smooth(data=neutral2, aes(group=X.run.number., x=X.step., y=pro.behavior), se = FALSE, color = "gray20") + # Remove hashtag to view all individual lines.
  xlab("Step") +
  ylab("Proportion of pro-environmental behaviors") +
  ggtitle("Time series of pro-environmental behavior", subtitle = 
            "50% pro-environmental affordances\n300 runs without niche construction") +
  coord_cartesian(ylim = c(0, 1), xlim =c(0,2000)) 

pn2

# PLOT OF MEAN PRO-ENVIRONMENTAL BEHAVIOR OVER TIME FOR 60% AFFORDANCE (WITH NICHE CONSTRUCTION)

pb <- ggplot() + 
  geom_smooth(data=means.bias, aes(x=X.step., y=meanpro), color = "gray20", se = FALSE, size = 0.5) +
  geom_ribbon(data=means.bias, aes(ymin=meanpro-sdpro, ymax=meanpro+sdpro, x=X.step., fill = "band"), alpha = 0.2, fill = "gray20")+
#  geom_smooth(data=bias, aes(group=X.run.number., x=X.step., y=pro.behavior), color = "gray20", se = FALSE) + # Remove hashtag to view all individual lines.
  xlab("Step") +
  ylab("Proportion of pro-environmental behaviors") +
  ggtitle("Time series of pro-environmental behavior", subtitle = 
            "60% pro-environmental affordances\n300 runs with niche construction") +
  coord_cartesian(ylim = c(0.5, 1), xlim =c(0,2000)) 

pb

# PLOT OF MEAN PRO-ENVIRONMENTAL BEHAVIOR OVER TIME FOR 60% AFFORDANCE (WITHOUT NICHE CONSTRUCTION)

pb2 <- ggplot() + 
  geom_smooth(data=means.bias2, aes(x=X.step., y=meanpro), color = "gray20", se = FALSE, size = 0.5) +
  geom_ribbon(data=means.bias2, aes(ymin=meanpro-sdpro, ymax=meanpro+sdpro, x=X.step., fill = "band"), alpha = 0.2, fill = "gray20")+
  #  geom_smooth(data=bias2, aes(group=X.run.number., x=X.step., y=pro.behavior), color = "gray20", se = FALSE) + # Remove hashtag to view all individual lines.
  xlab("Step") +
  ylab("Proportion of pro-environmental behaviors") +
  ggtitle("Time series of pro-environmental behavior", subtitle = 
            "60% pro-environmental affordances\n300 runs without niche construction") +
  coord_cartesian(ylim = c(0.5, 1), xlim =c(0,2000)) 

pb2


################################################
##    END OF STYLIZED MODEL ANALYSIS
################################################

################################################
##    START OF OF COPENHAGEN MODEL ANALYSIS (EMPIRICAL VALIDATION)
################################################

##################################
##    COPENHAGEN REAL-WORLD DATA
##################################

# READ DATA (DATA ACQUIRED FROM THE CITY OF COPENHAGEN, PERSONAL COMMUNICATION)
dataBC = read.csv("https://raw.githubusercontent.com/roopekaaronen/affordance/master/bikes-cars.csv", stringsAsFactors = FALSE)

# PLOT DRIVING AND CYCLING BEHAVIORS OVER TIME

pBC <- ggplot(dataBC) + 
  geom_line(aes(x=Year, y=Vehicles..thousand., col = "Vehicle"), fill = "gray20", size = 0.5) +
  geom_line(aes(x=Year, y=Bicycles..thousand., col = "Bicycle"), fill = "firebrick3", size = 0.5) +
  xlab("Year") +
  ylab("Amount (thousands)") +
  ggtitle("Traffic crossing the city center in Copenhagen\nReal-world data, 1970-2018") +
  ylim(0, 550) +
  scale_colour_manual(name="Behavior",
                      values=c(Bicycle="gray20", Vehicle="firebrick3"))

## PRINT PLOT
pBC

# CREATE DATA FOR DEVELOPMENT OF CYCLE TRACKS IN COPENHAGEN (DATA FROM CITY OF COPENHAGEN)
cphbike <- c(294, 302, 307, 323, 329, 332, 338, 346, 359, 368, 375, 382)
cphbike <- data.frame(cphbike)
cphbike$year <- c(1996, 1998, 2000, 2002, 2004, 2006, 2008, 2010, 2012, 2014, 2016, 2018)

# PLOT CYCLE TRACKS
cphplot <- ggplot(cphbike) +
  geom_point(aes(year, cphbike), size = 0.5) +
  geom_smooth(aes(year, cphbike), size = 0.5, se = FALSE, color = "grey20", method = "lm") +
  xlab("Year") +
  ylab("Cycle tracks (km)") +
  ggtitle("Kilometres of bicycle tracks in Copenhagen\nReal-world data by year (1996-2018)")
cphplot

##################################
##    COPENHAGEN SIMULATED DATA
##################################

# DOWNLOAD FILE FROM https://github.com/roopekaaronen/affordance/blob/master/Copenhagen.timeseries300.csv.zip?raw=true
# READ DATA
dataC = read.csv("C:/Users/RoopeOK/Documents/Yliopisto/Affordance model/Analysis/CSV//Copenhagen.timeseries300.csv", skip = 6, stringsAsFactors = FALSE)

# TRANSFORM TOTAL NUMBER OF BEHAVIOR INTO PROPORTION
dataC$pro.behavior <- dataC$pro.behavior/dataC$number.of.agents
dataC$non.behavior <- dataC$non.behavior/dataC$number.of.agents

# CREATE SUMMARY DATA (MEAN AND SD OF PRO-BEHAVIOR AND NON-BEHAVIOR, AS AFFORDANCES COMPOSITION)
data.means <- dataC %>%
  dplyr::group_by(X.step.) %>%
  dplyr::summarize(meanpro = mean(pro.behavior), meannon = mean(non.behavior), sdpro = sd(pro.behavior), 
                   sdnon = sd(non.behavior), meanaff = mean(count.patches.with..pcolor...violet.),
                   sdaff = sd(count.patches.with..pcolor...violet.))
data.means

## TRANSFORM TICKS INTO YEARS (ASSUMING 365-DAY YEARS)
data.means$X.step. <- (data.means$X.step. / 365)

## TRANSFORM YEARS INTO DATES, STARTING FROM 1970
data.means$X.step. <- data.means$X.step. + 1970  

## DELETE FIRST ROW (BLANK)
data.means <- data.means[-c(1),]

## PLOT SIMULATED MEAN PRO-ENVIRONMENTAL AND NON-ENVIRONMENTAL BEHAVIOR OVER TIME WITH +/- 1 SD RIBBON

p <- ggplot(data.means) + 
  geom_smooth(aes(y=meanpro, x=X.step., colour="Bicycle"), size = 0.5)+
  geom_ribbon(aes(ymin=meanpro-sdpro, ymax=meanpro+sdpro, x=X.step., fill = "band"), alpha = 0.2, fill = "gray20")+
  geom_smooth(aes(y=meannon, x=X.step., colour="Vehicle"), size = 0.5)+
  geom_ribbon(aes(ymin=meannon-sdnon, ymax=meannon+sdnon, x=X.step., fill = "band"), alpha = 0.2, fill = "firebrick3")+
  ylim(-0.02, 1.02) +
  xlim(1970, 2028) +
  ggtitle("Pro-environmental and non-environmental behavior\nSimulated time series (300 runs)") +
  xlab("Year") +
  ylab("Proportion of total agents") +
  geom_vline(xintercept = 2018, linetype = "longdash", colour = "gray20", size = 0.3) +
  scale_colour_manual(name="Behavior",
                      values=c(Bicycle="gray20", Vehicle="firebrick3"))
p

# SELECT A SINGLE RUN FROM THE TIME SERIES DATA
single <- dataC[ which(dataC$X.run.number.==9), 1:8] # RUN NUMBER CAN BE CHANGED TO ANY VALUE BETWEEN 1 AND 300

# TRANSFORM TICKS INTO YEARS AND YEARS INTO DATES
single$X.step. <- (single$X.step. / 365)
single$X.step. <- single$X.step. + 1970  
single <- single[-c(1),]

# PLOT THE SINGLE RUN
# tiff("Figure17.tiff", units="mm", width=85, height=85, res=300)
ps <- ggplot(single) + 
  geom_smooth(aes(y=pro.behavior, x=X.step., colour="Bicycle"), size = 0.5)+
  geom_smooth(aes(y=non.behavior, x=X.step., colour="Vehicle"), size = 0.5)+
  geom_line(aes(y=pro.behavior, x=X.step., colour="Bicycle"), alpha = 0.5, size = 0.3)+
  geom_line(aes(y=non.behavior, x=X.step., colour="Vehicle"), alpha = 0.5, size = 0.3)+
  ylim(-0.02, 1.02) +
  xlim(1970, 2028) +
  ggtitle("Pro-environmental and non-environmental behavior\nSimulated time series (selected single run)") +
  xlab("Year") +
  ylab("Proportion of total agents") +
  geom_vline(xintercept = 2018, linetype = "longdash", colour = "gray20", size = 0.3) +
  scale_colour_manual(name="Behavior",
                      values=c(Bicycle="gray20", Vehicle="firebrick3"))
# PRINT PLOT
ps


# PLOT PRO-ENVIRONMENTAL AFFORDANCES OVER TIME

# FIRST TRANSFORM TOTAL NUMBER OF AFFORDANCES INTO PROPORTIONS 
data.means$meanaff <- data.means$meanaff/40401 # 40401 IS THE TOTAL AMOUNT OF PATCHES IN THE MODEL
data.means$sdaff <- data.means$sdaff/40401

# CREATE GGPLOT
pA <- ggplot(data.means) + 
  geom_smooth(aes(y=meanaff, x=X.step.), color = "grey20", size = 0.5)+
  geom_ribbon(aes(ymin=meanaff-sdaff, ymax=meanaff+sdaff, x=X.step., fill = "band"), alpha = 0.2, fill = "gray20")+
  ggtitle("Proportion of pro-environmental affordances\nSimulated time series") +
  xlab("Year") +
  ylab("Proportion of pro-environmental affordances") +
  geom_vline(xintercept = 2018, linetype = "longdash", colour = "gray20", size = 0.3)
# coord_cartesian(ylim =c(0.5,0.8), xlim =c(1996,2018)) # zoom into the data
# PRINT PLOT
pA

##################################
##    COPENHAGEN BATCH ANALYSIS
##################################

# READ DATA
dataTB = read.csv("https://raw.githubusercontent.com/roopekaaronen/affordance/master/Copenhagen.batch.csv", skip = 6, stringsAsFactors = FALSE)
datTB <- data.frame(dataTB)

# TRANSFORM TOTAL NUMBER OF BEHAVIOR INTO PROPORTION
datTB$pro.behavior <- datTB$pro.behavior/datTB$number.of.agents
datTB$non.behavior <- datTB$non.behavior/datTB$number.of.agents

# GROUP DATA BY PRO-ENVIRONMENTAL NICHE CONSTRUCTION RATE
aggTB <- datTB %>%
  group_by(construct.pro)

# PLOT RATE OF PRO-ENVIRONMENTAL NICHE CONSTRUCTION BY ENVIRONMENTAL BEHAVIORS
pTB <- ggplot(data = aggTB, aes(x = construct.pro)) +
  geom_point(aes(construct.pro, pro.behavior, colour="Bicycle"), alpha = 0.2, size = 0.3) +
  geom_point(aes(construct.pro, non.behavior, colour="Vehicle"), alpha = 0.2, size = 0.3) +
  geom_smooth(aes(construct.pro, pro.behavior, colour="Bicycle"), se = FALSE, method = "auto", span = 0.9, show.legend = F, colour = "gray20", size = 0.5) +
  geom_smooth(aes(construct.pro, non.behavior, colour="Vehicle"), se = F, method = "auto", span = 0.9, show.legend = F, colour = "firebrick3", size = 0.5) +
  xlab("Rate of pro-environmental niche construction") +
  ylab("Proportion of total agents") +
  ggtitle("Behavior as a function of niche construction", subtitle = "At step 17885 (or year 2018)") +
  scale_colour_manual(name="Behavior",
                      values=c(Bicycle="gray20", Vehicle="firebrick3")) +
  guides(colour = guide_legend(override.aes = list(size=2)))

# PRINT PLOT
pTB
################################################
##    END OF COPENHAGEN MODEL ANALYSIS
################################################


################################################
##    CLUSTER ANALYSIS
##    Note: This code is adapted from https://rpubs.com/gabrielmartos/ClusterAnalysis
################################################

# SELECT VARIABLES OF INTEREST INTO NEW OBJECT
mydata <- aggB %>% dplyr::select(17, 23)

# STANDARDIZE DATA
mydata.stand <- scale(mydata)

# CALCULATE K.MEANS
k.means.fit <- kmeans(mydata.stand, 2) # k = 2

# PLOT AND PRINT AN ELBOW PLOT
wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

wssplot(mydata.stand, nc=6)

## VISUALISE THE CLUSTERS WITH 95% ELLIPSES

pCluster <- fviz_cluster(object = k.means.fit, data = mydata.stand, geom = "point", pointsize = 0.3, choose.vars = c("pro.amount", "pro.behavior"), ellipse.type = "norm", ellipse.level
= 0.95) + theme_bw(base_size = 6) + 
ggtitle("Clusters of pro-environmental behavior by proportion of affordances", 
        subtitle = "With niche construction") +
  xlab("Proportion of pro-environmental affordance") +
  ylab("Proportion of pro-environmental behavior")
pCluster

################################################
##    END OF CLUSTER ANALYSIS
################################################

################################################
##    START OF NETWORK STRUCTURE
################################################

# READ DATA
dataN = read.csv("https://raw.githubusercontent.com/roopekaaronen/affordance/master/network.csv", skip = 6, stringsAsFactors = FALSE)
dataN <- dataN[ which(dataN$X.step. ==0), 1:21]

# PASTE DATA OF ALL LINK NEIGHBORS AND TRANSFORM INTO NUMERIC
test <- paste(dataN$link.neighbors[1:1000], collapse=",")
test <- as.numeric( unlist ( str_split (test, " ")))

# CREATE DATA-FRAME
test2 <- data.frame(test)

# DATA OF NETWORK STRUCTURE OF A MANUALLY SELECTED SINGLE MODEL RUN
net <- c(7, 9, 5, 10, 5, 6, 9, 5, 13, 6, 7, 21, 5, 11, 5, 21, 9, 7, 16, 6, 5, 8, 11, 17, 7, 7, 6, 9, 7, 5, 16, 10, 5, 6, 6, 10, 38, 12, 12, 18, 5, 5, 12, 5, 7, 7, 5, 8, 6, 5, 6, 5, 9, 11, 5, 8, 5, 5, 7, 5, 6, 8, 6, 6, 5, 6, 5, 7, 10, 16, 6, 10, 5, 7, 5, 50, 10, 6, 6, 6, 10, 7, 6, 5, 130, 7, 5, 8, 7, 5, 6, 7, 10, 6, 9, 7, 6, 13, 5, 11, 9, 13, 7, 5, 6, 6, 6, 7, 9, 10, 9, 5, 12, 96, 6, 27, 5, 15, 71, 11, 5, 5, 18, 5, 5, 6, 12, 10, 14, 7, 8, 8, 7, 14, 10, 12, 12, 6, 10, 8, 5, 5, 9, 17, 21, 5, 7, 12, 6, 5, 37, 7, 5, 12, 8, 8, 6, 6, 5, 6, 17, 5, 5, 5, 7, 6, 6, 27, 10, 5, 8, 6, 6, 5, 5, 7, 7, 19, 5, 7, 11, 6, 5, 5, 16, 17, 13, 13, 15, 5, 6, 5, 10, 7, 7, 14, 6, 7, 5, 5, 6, 8, 18, 9, 9, 5, 7, 6, 6, 6, 9, 6, 13, 14, 7, 6, 6, 19, 8, 8, 5, 5, 7, 13, 5, 5, 7, 6, 8, 5, 8, 6, 14, 12, 5, 10, 7, 7, 6, 7, 5, 5, 6, 10, 8, 7, 7, 5, 6, 6, 5, 116, 6, 5, 9, 15, 22, 6, 5, 7, 6, 5, 10, 6, 6, 15, 7, 8, 5, 5, 7, 11, 7, 6, 10, 11, 6, 9, 11, 5, 12, 9, 5, 6, 13, 7, 5, 9, 12, 9, 5, 8, 5, 29, 9, 11, 9, 6, 5, 13)
net <- data.frame(net)


# PLOT MANUALLY SELECTED SINGLE MODEL RUN
tiff("FigureS2.tiff", units="mm", width=85, height=85, res=300)

degreeplot1 <- ggplot(net, aes(x = net)) +
  geom_histogram(binwidth = 1) +
  scale_x_continuous(breaks = round(seq(min(net$net), max(net$net), by = 10),1)) +
  xlab("Number of links") +
  ylab("Frequency") +
  ggtitle("Network degree distribution") +
  geom_vline(xintercept=mean(net$net), linetype = "longdash", colour = "gray20", size = 0.5) +
  geom_vline(xintercept=median(net$net), linetype = "longdash", color="firebrick3", size = 0.5)

# PRINT PLOT
degreeplot1
dev.off()

# PLOT NETWORK STRUCTURE OF ALL 1000 SIMULATIONS (WITH LOG SCALE)
tiff("FigureS3.tiff", units="mm", width=85, height=85, res=300)

degreeplot2 <- ggplot(test2, aes(x = test)) +
  geom_histogram(binwidth = 1) +
  xlab("Number of links") +
  ylab("Frequency (log10)") +
  ggtitle("Network degree distribution", subtitle = "1000 simulations; 300,000 agents") +
  geom_vline(xintercept=mean(net$net), linetype = "longdash", color="gray20", size = 0.5)  +
  geom_vline(xintercept=median(net$net), linetype = "longdash", color="firebrick3", size = 0.5)  +
  scale_y_continuous(trans='log10', breaks = c(1, 2, 5, 10, 100, 1000, 10000, 100000)) +
  scale_x_continuous(breaks = c(1, 50, 100, 150, 200, 250, 275))

# PRINT PLOT
degreeplot2
dev.off()

# PLOT GLOBAL CLUSTERING COEFFICIENT OF ALL 1000 SIMULATIONS
tiff("FigureS4.tiff", units="mm", width=85, height=85, res=300)

clustercoeff <- ggplot(dataN, aes(x = dataN$global.clustering.coefficient)) +
  geom_histogram(binwidth = 0.01) +
  scale_x_continuous(breaks = round(seq(min(dataN$global.clustering.coefficient), max(dataN$global.clustering.coefficient), by = 0.01),1)) +
  xlab("Global clustering coefficient") +
  ylab("Frequency") +
  ggtitle("Global clustering coefficient, 1000 simulations") +
  geom_vline(xintercept=mean(dataN$global.clustering.coefficient), linetype = "longdash", color="gray20", size = 0.5)

# PRINT PLOT
clustercoeff
dev.off()
# CALCULATE MEAN GLOBAL CLUSTERING COEFFICIENT OF 1000 SIMS
mean(dataN$global.clustering.coefficient)

################################################
##    END OF NETWORK STRUCTURE
################################################

################################################
##    START OF SENSITIVITY TESTING
################################################

################################################
##    START OF LOCAL SENSITIVITY TESTING
################################################

################################################
##    RAINCLOUD PLOTS (ALLEN, 2018)
##    CODE FOR RAINDCLOUD PLOTS IS ADAPTED FROM ALLEN, 2018: https://micahallen.org/2018/03/15/introducing-raincloud-plots/
################################################
## NOTE: REQUIRES ALL PACKAGES INSTALLED AND LOADED (SEE BEGINNING OF SCRIPT)

## DEFINE THE THEME
raincloud_theme = theme(
  text = element_text(size = 6),
  axis.title.x = element_text(size = 7),
  axis.title.y = element_text(size = 7),
  axis.text = element_text(size = 6),
  axis.text.x = element_text(angle = 45, vjust = 0.5),
  legend.title=element_text(size=6),
  legend.text=element_text(size=6),
  legend.position = "right",
  plot.title = element_text(lineheight=.8, face="bold", size = 7),
  panel.border = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
  axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))

## SUMMARY STATISTICS
lb <- function(x) mean(x) - sd(x)
ub <- function(x) mean(x) + sd(x)

## SET SOURCE
source("https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")



## ====== SENSITIVITY FOR POPULATION SIZE =======

# READ DATA
senpop = read.csv("https://raw.githubusercontent.com/roopekaaronen/affordance/master/Sensitivity-pop.csv", skip = 6, stringsAsFactors = FALSE)

# STANDARDIZE AND TRANSFORM VARIABLES
senpop$pro.behavior <- senpop$pro.behavior/senpop$number.of.agents
senpop$number.of.agents <- as.factor(senpop$number.of.agents)

# CREATE SUMMARY STATISTICS
sumld <- ddply(senpop, ~number.of.agents, summarise, mean = mean(pro.behavior), median = median(pro.behavior), lower = lb(pro.behavior), upper = ub(pro.behavior))

# PLOT AND PRINT THE RAINCLOUD PLOT 

tiff("SensitivityTest5.tiff", units="mm", width=85, height=85, res=300)

g <- ggplot(data = senpop, aes(y = pro.behavior, x = number.of.agents, fill = number.of.agents)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8, size = 0.3) +
  geom_point(aes(y = pro.behavior, color = number.of.agents), position = position_jitter(width = .15), size = .3, alpha = 0.8) +
  geom_boxplot(width = .2, guides = FALSE, outlier.shape = NA, alpha = 0.8, size = 0.3) +
  expand_limits(x = 5.25) +
  guides(fill = FALSE) +
  guides(color = FALSE) +
  scale_color_brewer(palette = "Paired") +
  scale_fill_brewer(palette = "Paired") +
  coord_flip() +
  theme_bw() +
  raincloud_theme +
  xlab("Number of agents") +
  ylab("Proportion of pro-environmental behavior") +
  ggtitle("Sensitivity testing for number of agents\nOutput at step 2000")

g

dev.off()
##
## THE ABOVE IS REPEATED FOR EACH FREE PARAMETER
##
## ====== SENSITIVITY FOR NETWORK DENSITY =======

sennet = read.csv("https://raw.githubusercontent.com/roopekaaronen/affordance/master/Sensitivity-net.csv", skip = 6, stringsAsFactors = FALSE)

sennet$pro.behavior <- sennet$pro.behavior/sennet$number.of.agents
sennet$network.param <- as.factor(sennet$network.param)
sumld<- ddply(sennet, ~network.param, summarise, mean = mean(pro.behavior), median = median(pro.behavior), lower = lb(pro.behavior), upper = ub(pro.behavior))

tiff("SensitivityTest4.tiff", units="mm", width=85, height=85, res=300)

g2 <- ggplot(data = sennet, aes(y = pro.behavior, x = network.param, fill = network.param)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8, size = 0.3) +
  geom_point(aes(y = pro.behavior, color = network.param), position = position_jitter(width = .15), size = .3, alpha = 0.8) +
  geom_boxplot(width = .12, guides = FALSE, outlier.shape = NA, alpha = 0.8, size = 0.3) +
  expand_limits(x = 5.25) +
  guides(fill = FALSE) +
  guides(color = FALSE) +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  coord_flip() +
  theme_bw() +
  raincloud_theme +
  xlab("Minimun degree of connection") +
  ylab("Proportion of pro-environmental behavior") +
  ggtitle("Sensitivity testing for network density\nOutput at step 2000")

g2
dev.off()
## ====== SENSITIVITY FOR SOCIAL LEARNING =======

sensoc = read.csv("https://raw.githubusercontent.com/roopekaaronen/affordance/master/Sensitivity-soc.csv", skip = 6, stringsAsFactors = FALSE)
sensoc$pro.behavior <-sensoc$pro.behavior/sensoc$number.of.agents
sensoc$social.learning <- as.factor(sensoc$social.learning)

sumld<- ddply(sensoc, ~social.learning, summarise, mean = mean(pro.behavior), median = median(pro.behavior), lower = lb(pro.behavior), upper = ub(pro.behavior))

tiff("SensitivityTest7.tiff", units="mm", width=85, height=85, res=300)

g3 <- ggplot(data =sensoc, aes(y = pro.behavior, x = social.learning, fill = social.learning)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8, size = 0.3) +
  geom_point(aes(y = pro.behavior, color = social.learning), position = position_jitter(width = .15), size = .3, alpha = 0.8) +
  geom_boxplot(width = .12, guides = FALSE, outlier.shape = NA, alpha = 0.8, size = 0.3) +
  expand_limits(x = 5.25) +
  guides(fill = FALSE) +
  guides(color = FALSE) +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  coord_flip() +
  theme_bw() +
  raincloud_theme +
  xlab("Rate of social learning") +
  ylab("Proportion of pro-environmental behavior") +
  ggtitle("Sensitivity testing for social learning\nOutput at step 2000")

g3
dev.off()
## ====== SENSITIVITY FOR ENVIRONMENT STRUCTURE =======

senenv = read.csv("https://raw.githubusercontent.com/roopekaaronen/affordance/master/Sensitivity-env.csv", skip = 6, stringsAsFactors = FALSE)
senenv$pro.behavior <-senenv$pro.behavior/senenv$number.of.agents
senenv$pro.amount <- as.factor(senenv$pro.amount)

sumld<- ddply(senenv, ~pro.amount, summarise, mean = mean(pro.behavior), median = median(pro.behavior), lower = lb(pro.behavior), upper = ub(pro.behavior))

tiff("SensitivityTest1.tiff", units="mm", width=85, height=85, res=300)

g4 <- ggplot(data =senenv, aes(y = pro.behavior, x = pro.amount, fill = pro.amount)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8, size = 0.3) +
  geom_point(aes(y = pro.behavior, color = pro.amount), position = position_jitter(width = .15), size = .3, alpha = 0.5) +
  geom_boxplot(width = .25, guides = FALSE, outlier.shape = NA, alpha = 0.8, size = 0.3) +
  expand_limits(x = 5.25) +
  guides(fill = FALSE) +
  guides(color = FALSE) +
  scale_color_brewer(palette = "Paired") +
  scale_fill_brewer(palette = "Paired") +
  coord_flip() +
  theme_bw() +
  raincloud_theme +
  xlab("Proportion of pro-environmental affordance") +
  ylab("Proportion of pro-environmental behavior") +
  ggtitle("Sensitivity testing for proportion of affordances\nOutput at step 2000")

g4
dev.off()
## ====== SENSITIVITY FOR NICHE CONSTRUCTION =======

sennc = read.csv("https://raw.githubusercontent.com/roopekaaronen/affordance/master/Sensitivity-nc.csv", skip = 6, stringsAsFactors = FALSE)
sennc$pro.behavior <-sennc$pro.behavior/sennc$number.of.agents
sennc$construct.pro <- as.factor(sennc$construct.pro)

sumld<- ddply(sennc, ~construct.pro, summarise, mean = mean(pro.behavior), median = median(pro.behavior), lower = lb(pro.behavior), upper = ub(pro.behavior))

tiff("SensitivityTest3.tiff", units="mm", width=85, height=85, res=300)

g5 <- ggplot(data =sennc, aes(y = pro.behavior, x = construct.pro, fill = construct.pro)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8, size = 0.3) +
  geom_point(aes(y = pro.behavior, color = construct.pro), position = position_jitter(width = .15), size = .3, alpha = 0.5) +
  geom_boxplot(width = .15, guides = FALSE, outlier.shape = NA, alpha = 0.8, size = 0.3) +
  expand_limits(x = 5.25) +
  guides(fill = FALSE) +
  guides(color = FALSE) +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  coord_flip() +
  theme_bw() +
  raincloud_theme +
  xlab("Rate of pro-environmental niche construction") +
  ylab("Proportion of pro-environmental behavior") +
  ggtitle("Sensitivity testing for niche construction\nOutput at step 2000")

g5
dev.off()
## ====== SENSITIVITY FOR ASOCIAL LEARNING =======

senasoc = read.csv("https://raw.githubusercontent.com/roopekaaronen/affordance/master/Sensitivity-asoc.csv", skip = 6, stringsAsFactors = FALSE)
senasoc$pro.behavior <-senasoc$pro.behavior/senasoc$number.of.agents
senasoc$asocial.learning <- as.factor(senasoc$asocial.learning)

sumld<- ddply(senasoc, ~asocial.learning, summarise, mean = mean(pro.behavior), median = median(pro.behavior), lower = lb(pro.behavior), upper = ub(pro.behavior))

tiff("SensitivityTest2.tiff", units="mm", width=85, height=85, res=300)

g6 <- ggplot(data =senasoc, aes(y = pro.behavior, x = asocial.learning, fill = asocial.learning)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8, size = 0.3) +
  geom_point(aes(y = pro.behavior, color = asocial.learning), position = position_jitter(width = .15), size = .3, alpha = 0.8) +
  geom_boxplot(width = .15, guides = FALSE, outlier.shape = NA, alpha = 0.8, size = 0.3) +
  expand_limits(x = 5.25) +
  guides(fill = FALSE) +
  guides(color = FALSE) +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  coord_flip() +
  theme_bw() +
  raincloud_theme +
  xlab("Rate of asocial learning") +
  ylab("Proportion of pro-environmental behavior") +
  ggtitle("Sensitivity testing for asocial learning\nOutput at step 2000")

g6
dev.off()
## ====== SENSITIVITY FOR PERSONAL STATE =======

senpro = read.csv("https://raw.githubusercontent.com/roopekaaronen/affordance/master/Sensitivity-pro.csv", skip = 6, stringsAsFactors = FALSE)
senpro$pro.behavior <-senpro$pro.behavior/senpro$number.of.agents
senpro$initial.pro <- as.factor(senpro$initial.pro)

sumld<- ddply(senpro, ~initial.pro, summarise, mean = mean(pro.behavior), median = median(pro.behavior), lower = lb(pro.behavior), upper = ub(pro.behavior))

tiff("SensitivityTest6.tiff", units="mm", width=85, height=85, res=300)

g7 <- ggplot(data =senpro, aes(y = pro.behavior, x = initial.pro, fill = initial.pro)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8, size = 0.3) +
  geom_point(aes(y = pro.behavior, color = initial.pro), position = position_jitter(width = .15), size = .3, alpha = 0.8) +
  geom_boxplot(width = .2, guides = FALSE, outlier.shape = NA, alpha = 0.8, size = 0.3) +
  expand_limits(x = 5.25) +
  guides(fill = FALSE) +
  guides(color = FALSE) +
  scale_color_brewer(palette = "Paired") +
  scale_fill_brewer(palette = "Paired") +
  coord_flip() +
  theme_bw() +
  raincloud_theme +
  xlab("Mean initial personal state") +
  ylab("Proportion of pro-environmental behavior") +
  ggtitle("Sensitivity testing for personal state\nOutput at step 2000")

g7
dev.off()

################################################
##    END OF LOCAL SENSITIVITY TESTING
################################################

################################################
##    START OF GLOBAL SENSITIVITY TESTING
##    LATIN-HYPERCUBE SAMPLING
##    THE FOLLOWING CODE IS ADAPTED FROM: https://ropensci.github.io/nlrx/articles/simdesign-examples.html#latin-hypercube-sampling-simdesign_lhs
################################################

##################################
##    CREATE LATIN HYPERCUBE SAMPLES
##################################
# I ADVISE NOT RUNNING THE CODE SEQUENCE BETWEEN THE FOLLOWING TWIN ASTERISK LINES, UNLESSY YOU KNOW WHAT YOU ARE DOING!
# THIS PART OF THE CODE ALSO REQUIRES THAT JAVA DEVELOPMENT KIT IS INSTALLED (SEE LINK ABOVE FOR INSTRUCTIONS).
# **********************************************************************************************************
# **********************************************************************************************************

# devtools::install_github("ropensci/nlrx") # THIS NEEDS TO BE UNCOMMENTED IF RUN FOR THE FIRST TIME

# Windows default NetLogo installation path (adjust to your needs!):
netlogopath <- file.path("C:/Program Files/NetLogo 6.0.4")
modelpath <- file.path("C:/DirectoryOfYourModel")
outpath <- file.path("C:/DirectoryOfYourOutput")


nl <- nl(nlversion = "6.0.4",
         nlpath = netlogopath,
         modelpath = modelpath,
         jvmmem = 1024)
?experiment
nl@experiment <- experiment(expname="affordance",
                            outpath="C:/DirectoryOfYourOutput",
                            repetition=1,
                            tickmetrics="false",
                            idsetup="setup",
                            idgo="go",
                            idfinal=NA_character_,
                            idrunnum=NA_character_,
                            runtime=2000,
                            evalticks=seq(2000),
                            metrics=c("pro-behavior"),
                            variables = list('number-of-agents' = list(min=100, max=1000, qfun="qunif"),
                                             'pro-amount' = list(min=0.33, max=0.66, qfun="qunif"),
                                             'initial-pro' = list(min=0.33, max=0.66, qfun="qunif"),
                                             'initial-non' = list(min=0.33, max=0.66, qfun="qunif"),
                                             'construct-pro' = list(min=0, max=10, qfun="qunif"),
                                             'construct-non' = list(min=0, max=10, qfun="qunif"),
                                             'social-learning' = list(min=0.0002, max=0.0008, qfun="qunif"),
                                             'asocial-learning' = list(min=0.0002, max=0.0008, qfun="qunif"),
                                             'network-param' = list(min=3, max=7, qfun="qunif")),
                            constants = list("network-type" = "\"KE\"",
                                             "mu" = 0.9,
                                             "mutate-on?" = "false",
                                             "rseed" = 0,
                                             "random-seed?" = "false",
                                             "networks" = "true",
                                             "niche-construction" = "true"))



nl@simdesign <- simdesign_lhs(nl=nl,
                              samples=300,
                              nseeds=5,
                              precision=5)

results <- run_nl_all(nl)

# Attach results to nl object:
setsim(nl, "simoutput") <- results

# Write output to outpath of experiment within nl
write_simoutput(nl)

# Do further analysis:
analyze_nl(nl)

results$`pro-behavior` <- results$`pro-behavior`/results$`number-of-agents`
View(results)

# **********************************************************************************************************
# **********************************************************************************************************

##################################
##    READ AND PLOT DATA FROM LATIN HYPERCUBE SAMPLE RUNS
##################################

results = read.csv("https://raw.githubusercontent.com/roopekaaronen/affordance/master/affordance_lhs.csv", stringsAsFactors = FALSE)

# TRANSFORM TOTAL NUMBER OF BEHAVIORS TO PROPORTIONS
results$pro.behavior <- results$pro.behavior/results$number.of.agents

# CREATE SUMMARY DATA
results.sum <- results %>% 
  group_by(siminputrow) %>%
  dplyr::summarize(mean = mean(pro.behavior), min = min(pro.behavior), max = max(pro.behavior))

# PLOT GLOBAL SENSITIVITY ANALYSIS BY SIMULATION NUMBER AND PRO-BEHAVIOR
tiff("SensitivityTest8.tiff", units="mm", width=85, height=85, res=300)


w <- ggplot(results.sum) +
  geom_point(data = results, aes(siminputrow, pro.behavior), alpha = 0.8, color = "gray20", size = 0.7) +
geom_errorbar(data = results.sum, aes(siminputrow, ymin = min, ymax=  max), width=0.5, colour = "gray20", size = 0.3) +
  ggtitle("Global sensitivity test, all parameters") +
  xlab("Simulation number") +
  ylab("Proportion of pro-environmental behavior")
w  
dev.off()
# PLOT GLOBAL SENSITIVITY ANALYSIS BY PRO-AMOUNT AND PRO-BEHAVIOR
tiff("SensitivityTest9.tiff", units="mm", width=85, height=85, res=300)

pra <- ggplot(results) + 
  geom_point(aes(y=pro.behavior, x=pro.amount), color = "grey20", alpha = 0.8, size = 0.8)+
  ggtitle("Global sensitivity test, pro-amount") +
  xlab("Proportion of pro-environmental affordance") +
  ylab("Proportion of pro-environmental behavior")
pra
dev.off()
# PLOT GLOBAL SENSITIVITY ANALYSIS BY INITIAL-PRO AND PRO-BEHAVIOR
tiff("SensitivityTest10.tiff", units="mm", width=85, height=85, res=300)

prba <- ggplot(results) + 
  geom_point(aes(y=pro.behavior, x=initial.pro), color = "grey20", alpha = 0.8, size = 0.8)+
 # geom_smooth(aes(y=pro.behavior, x=initial.pro, color = "initial.pro"), method='lm',formula=y~x)+
  ggtitle("Global sensitivity test, initial.pro") +
  xlab("Initial pro-environmental personal state") +
  ylab("Proportion of pro-environmental behavior") +
  geom_rect(aes(xmin = 0.55, xmax = 0.66, ymin = 0.05, ymax = 0.25),
            fill = "transparent", color = "firebrick3", size = 0.5)

prba
dev.off()
# PLOT GLOBAL SENSITIVITY ANALYSIS BY INITIAL-NON AND PRO-BEHAVIOR
prbb <- ggplot(results) + 
  geom_point(aes(y=pro.behavior, x=initial.non, color = "initial.non"), alpha = 0.8)+
  # geom_smooth(aes(y=pro.behavior, x=`initial-pro`, color = "initial.pro"), method='lm',formula=y~x)+
  ggtitle("Global sensitivity test, initial.non") +
  xlab("Initial non-environmental personal state") +
  ylab("Proportion of pro-environmental behavior") +
  scale_colour_manual(name="Personal state",
                      values=c(initial.pro="gray20", initial.non="firebrick3"))
prbb

# PLOT GLOBAL SENSITIVITY ANALYSIS BY CONSTRUCT-PRO AND PRO-BEHAVIOR
prca <- ggplot(results) + 
  geom_point(aes(y=pro.behavior, x=construct.pro), color = "gray20", alpha = 0.8)+
  ggtitle("Global sensitivity test, construct.pro") +
  xlab("Rate of niche construction") +
  ylab("Proportion of pro-environmental behavior")
prca

# PLOT GLOBAL SENSITIVITY ANALYSIS BY CONSTRUCT-NON AND PRO-BEHAVIOR
prcb <- ggplot(results) + 
  geom_point(aes(y=pro.behavior, x=construct.non), color = "gray20", alpha = 0.8)+
  ggtitle("Global sensitivity test, construct.non") +
  xlab("Rate of niche construction") +
  ylab("Proportion of pro-environmental behavior")
prcb


# PLOT GLOBAL SENSITIVITY ANALYSIS BY ASOCIAL-LEARNING AND PRO-BEHAVIOR
prda <- ggplot(results) + 
  geom_point(aes(y=pro.behavior, x=asocial.learning), color = "gray20", alpha = 0.8)+
  ggtitle("Global sensitivity test, asocial-learning") +
  xlab("Rate of learning") +
  ylab("Proportion of pro-environmental behavior")
prda

# PLOT GLOBAL SENSITIVITY ANALYSIS BY SOCIAL-LEARNING AND PRO-BEHAVIOR
prdb <- ggplot(results) + 
  geom_point(aes(y=pro.behavior, x=social.learning), color = "gray20", alpha = 0.8)+
  ggtitle("Global sensitivity test, social-learning") +
  xlab("Rate of learning") +
  ylab("Proportion of pro-environmental behavior")
prdb

# PLOT GLOBAL SENSITIVITY ANALYSIS BY NUMBER-OF-AGENTS AND PRO-BEHAVIOR
pre <- ggplot(results) + 
  geom_point(aes(y=pro.behavior, x=number.of.agents), color = "grey20", alpha = 0.8)+
  ggtitle("Global sensitivity test, number-of-agents") +
  xlab("Number of agents") +
  ylab("Proportion of pro-environmental behavior")
pre

prf <- ggplot(results) + 
  geom_point(aes(y=pro.behavior, x=network.param), color = "grey20", alpha = 0.8)+
  ggtitle("Global sensitivity test, network-param") +
  xlab("Minimun network density") +
  ylab("Proportion of pro-environmental behavior")
prf

################################################
##    END OF GLOBAL SENSITIVITY ANALYSIS
################################################

# PRINT PLOTS AS TIFF
## I use the tiff() command followed by dev.off() to print out high-resolution images to the working directory

tiff("Figure2.tiff", units="mm", width=150, height=85, res=300)
ggarrange(pB, pB2,
          labels = c("A", "B"),
          ncol = 2, nrow = 1,
          common.legend = TRUE, legend = "bottom")
dev.off()

tiff("Figure3.tiff", units="mm", width=95, height=85, res=300)
pCluster
dev.off()

tiff("Figure4.tiff", units="mm", width=150, height=140, res=300)
ggarrange(pn, pn2, pb, pb2,
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2,
          common.legend = TRUE) + 
  theme(legend.position = "bottom")
dev.off()


tiff("Figure5.tiff", units="mm", width=150, height=140, res=300)
ggarrange(pBC, p, ps, pTB,
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2,
          common.legend = TRUE) + 
  theme(legend.position = "bottom")
dev.off()

tiff("Figure6.tiff", units="mm", width=150, height=85, res=300)
ggarrange(cphplot, pA, 
          labels = c("A", "B"),
          ncol = 2, nrow = 1)
dev.off()
################################################
##    THE END!
################################################

################################################################
##    Roope Kaaronen   // University of Helsinki // IIASA
##    Date: 27.11.2019
##    Contact: roope dot kaaronen at helsinki dot fi
##    https://roopekaaronen.com
##    @roopekaaronen
##    GitHub: https://github.com/roopekaaronen
##    ROOPE KAARONEN, 2019
################################################################