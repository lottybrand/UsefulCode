##### Useful code!! #####
##### Bits of code that I've used in the past that will probably be useful in the future #####

##### Basics? #####
#set working directory
setwd("~/Desktop/")

#when plots are being weird:
dev.off()

# read text file
data <- read.delim("data.txt")

# read csv file
data <- read.csv("data.csv")

#save and write to new file (USE row.names = FALSE to avoid the extra X column!)
write.csv(df, file="somedf.csv", row.names=FALSE)

#save R objects
saveRDS(x, "x")

#load a data object you saved 
load("somedf.RDS")

x <- readRDS("x")


##### creating subsets and finding sample sizes in subsets etc #####
#examples here include participants per condition, per sex, etc

pptData <- tempData[!duplicated(tempData$ID),]

cond1 <- pptData[pptData$CONDITION==1,]
nrow(cond1)
cond2 <- pptData[pptData$CONDITION==3,]
nrow(cond2)
control <- pptData[pptData$CONDITION==2,]
nrow(control)
nrow(pptData)

#sample size per condition per sex calculations 

sampleSize <- qData[!duplicated(qData$ID),]

ssCondition <- sampleSize$CONDITION
table(ssCondition)

FsampleSize <- sampleSize[(sampleSize$SEX==2),]
table(FsampleSize$CONDITION)
MsampleSize <- sampleSize[(sampleSize$SEX==1),]
table(MsampleSize$CONDITION)

##### find and remove empty ghost cells that aren't NAs #####
any_empty <- ifelse((prolificIDs$ITT_1_A ==""),prolificIDs$ID,"OK")
table(any_empty)
empties <- any_empty[!any_empty=="OK"]
prolificIDs <- prolificIDs[!(prolificIDs$ID%in%empties),]


##### simple conversion!! ####

junk$nm[junk$nm == 11 ] <- 10 

# this doesn't always work
junk$nm[junk$nm == "B"] <- "b"

pro_ten$ResponseID[pro_ten$ResponseId =="R_23ZV6hyQvW9gcEu"] <- 11 

# simple string replacement! gsub

# replacing AGAINST with Anti in this column: 
FORpilotratings$rater_position <- gsub("AGAINST", "Anti", FORpilotratings$rater_position)

##### find column indexes:##### 
colnames(qData)

#rename column: 
colnames(qData)[8] <- "Qnumber"
colnames(df)[colnames(df) == 'oldName'] <- 'newName'

#replacing column names, e.g. making all "Copied" columns be called "copied" instead:
names(copywhoData) <- gsub("Copied", "copied", names(copywhoData))

##### using order: ####
#trying to re-order columns as numeric:
copywhoData <- copywhoData[order(as.numeric(names(copywhoData)[18:317]))]
# need to be careful here: 
col_names <- names(copywhoData[,colnames(copywhoData)[18:317]])
as.numeric(col_names)
sort(col_names)

#order
#re-order dataframe based on a column (look up 'order') df[order(x, -y, z), ]
tments <- tments[ order(tments$Treatment), ]


##### tapply: ####
#compute sum of scores per participant:
tapply(scoreCheck$score, list(scoreCheck$participant),sum)

#to create table of means per condition/sex etc
theMeansSwitching = tapply(qData$Switched, list(qData$CONDITION, qData$Sex),mean)
theMeansSwitching
table(Switching)

#remember na.rm = true
conf1Edu <- tapply(viruscraft$confidence1, list(viruscraft$higherEdu),mean, na.rm=TRUE)

##### create new dataframe manually (critical trials breakdown) ####

Trialtype <- c("All", "Critical","MajorityCorrect","MajorityIncorrect")  
totalTrials <- c(3120, 1524, 706, 818)
totalSwitched <- c(462, 428, 241, 187)
PropSwitched <- c(0.15, 0.28, 0.34, 0.23)
SocInfo <- (data.frame(Trialtype, totalTrials, totalSwitched, PropSwitched))


##### James'indexing code: #####
# used to rename ppt ID's from 1 to N consecutively with no gaps (for when ppt IDs aren't contiguous anymore)

Nppts = length(unique(myData$ID))
oldID <- myData$ID
contiguousID <- array(0,length(myData$ID))
for (index in 1:Nppts){
  contiguousID[oldID == unique(oldID)[index]] = index
}
myData$pptID <- contiguousID

##### dummy coding: making Condition A the baseline or "intercept" category. ##### 
Round3Summary$CondB <- ifelse(Round3Summary$Condition =="B", 1, 0)
Round3Summary$CondC <- ifelse(Round3Summary$Condition =="C", 1, 0)


##### using %in% and ifelse: #####
# make lists of correct answers 
correct1 <- c(1,4,7,9,10,12,14,16,18,19,21,23)                 
correct2 <- c(2,3,5,6,8,11,13,15,17,20,22,24)
qData$Correct <- ifelse((qData$Qnumber%in%correct1 & qData$Response==1),1,
                        ifelse((qData$Qnumber%in%correct2 & qData$Response==2),1,0))


##### coding qualtrics timing/timer columns as numeric: #####
# and dealing with the f*&^ng character class...
fckng_chars <- timing_clickbot[,grepl("timing|timer", colnames(timing_clickbot))]
col.num <- colnames(fckng_chars)
timing_clickbot[col.num] <- sapply(timing_clickbot[col.num],as.numeric)


##### reshape: #####

#simple reshape, just merging two columns (2 and 3) into one long column ("infectCorrect"), they get named via a "time" column, 1 and 2.
#you can then rename that time column (e.g. before and after playing viruscraft)
test1long <- reshape(test1, idvar = "ppt", 
                     varying = list(c(2,3)),
                     v.names = c("InfectCorrect"), 
                     direction = "long")


#more complex reshaping code, this takes the column numbers (8,13,18 etc) and puts them in a "Response" column,
#similarly, column numbers 9,14,19 etc are turned into a "Confidence" column. 
#This is taken from the confidence/conformity Qualtrics datafile "latestData"

qData <- reshape(qDataL, idvar = "ID", 
                 varying = list(c(8,13,18,23,28,33,38,43,48,53,58,63,68,73,78,83,88,93,98,103,108,113,118,123),
                                c(9,14,19,24,29,34,39,44,49,54,59,64,69,74,79,84,89,94,99,104,109,114,119,124),
                                c(10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100,105,110,115,120,125),
                                c(11,16,21,26,31,36,41,46,51,56,61,66,71,76,81,86,91,96,101,106,111,116,121,126),
                                c(12,17,22,27,32,37,42,47,52,57,62,67,72,77,82,87,92,97,102,107,112,117,122,127)),
                 v.names = c("Response", "Confidence","pAnswer","pSaw","pFinal"), 
                 direction = "long")

# wide to long! 

colnames(clean_clickbot)
long_clickbot <- reshape(clean_clickbot, 
                         idvar = "ID", 
                         varying = list(c("vax_att_1","vax_att_2","vax_att_3","vax_att_4","vax_att_5"),
                                        c("post_vax_att_1","post_vax_att_2","post_vax_att_3","post_vax_att_4","post_vax_att_5")),
                         v.names = c("pre_att", "post_att"), 
                         direction = "long")

# long to wide! 
# this one is taking the long_clickbot generated above and giving everyone just two ratings, before and after, for all 5 attitude types
# so you end up with n = 3580 (716*5)
wide_clickbot <- reshape(long_clickbot,  
                         timevar = c("post_rating"),
                         v.names = c("attitude"),
                         idvar = c("ID", "att_type"),
                         direction = "wide")

##### ifelse: #####
#relabel the choice column more usefully from the qualtrics output using ifelse 
prestigeData$choice <- ifelse((prestigeData$infoChoice=="Their score on the quiz"),"score",
                              ifelse((prestigeData$infoChoice=="One of their top three hobbies"),"hobbies",
                                     ifelse((prestigeData$infoChoice=="How many times they were copied"),"prestige","NA")))

# big nested ifelse statement (this is obviously stupid and should be a function):
d$GENRECOMB <- ifelse((grepl("pop", d$genre, ignore.case = TRUE)),"Pop",
                      ifelse((grepl("rock",d$genre, ignore.case = TRUE)), "Rock",
                             ifelse((grepl("hip hop",d$genre, ignore.case = TRUE)), "Hiphop",
                                    ifelse((grepl("metal",d$genre,ignore.case = TRUE)),"Metal",
                                           ifelse((grepl("alternative",d$genre,ignore.case = TRUE)),"Alternative",
                                                  ifelse((grepl("country",d$genre,ignore.case = TRUE)),"Country",
                                                         ifelse((grepl("christian",d$genre,ignore.case = TRUE)),"Christian",
                                                                ifelse((grepl("blues",d$genre, ignore.case = TRUE)),"Blues","Other"))))))))


##### grepl #####
# finding strings within strings (collaborations): 
collabs_XM <- artistClusterXM[(grepl(" and ", artistClusterXM$artist, ignore.case = TRUE) &
                                 (grepl(" feat. ", artistClusterXM$artist, ignore.case = TRUE))),]
collabSubsetXM <- data.frame(artist=collabs_XM)
View(collabSubsetXM)

# Likert responses translated to numbers (and 'as numeric' problem!) plus a neat test:
# REMEMBER! You can't just do a simple "as numeric" thing for Likert, because it'll number them alphabetically! Silly computer. 
pilot_ratings$response_ratings <- mapvalues(pilot_ratings$Response, from = c("Strongly Agree", "Agree", "Somewhat Agree", "Neither Agree nor Disagree",
                                                                             "Somewhat Disagree", "Disagree", "Strongly Disagree"),
                                            to = c(7,6,5,4,3,2,1))

pilot_ratings$response_ratings <- as.character(pilot_ratings$response_ratings)
pilot_ratings$response_ratings <- as.numeric(pilot_ratings$response_ratings)
check <- subset(pilot_ratings, select=c("Response","response_ratings"))
table(check)
#thank god for that
is.numeric(pilot_ratings$response_ratings)
#thank god for that!

##### substr #####
#convert columns to substring of their entry (e.g. "Scored 67/100" to "67")
scrdata <- read.delim("copyScore_16.02.18.txt")
newcolumns = list()
pos = 1 #counter for column index of new data frame
for(colindex in 6:105){
  newcolumns[[pos]] = sapply(scrdata[,colindex], function(x) substr(x, 8, 9))
  pos = pos+1
}
print(newcolumns)

NEWscrData <- scrdata
NEWscrData[, 6:105] <- newcolumns #replace the old columns with the new
scrdata <- NEWscrData

#using substring:
substr(SocInfo$Trialtype, 2, 20)
SocInfo$Trialtype <- substr(SocInfo$Trialtype, 2, 20)
#replace column names with substrings:
colnames(SocInfo) <- substr(colnames(SocInfo), 2, 20)

###### match is a very useful function. You can match entries/columns between dataframes. ##### 

#using match from the kernowResults file. I am finding the age of all ppts from the KernowResults file, and matching it based on their IDs in the KernowResults file: 
gPresD$Age <- kernowResults$Age[match(gPresD$rated_ID, kernowResults$ID)]
#check using order:
gPresD <- gPresD[order(gPresD$rated_ID),]
#works well, can do for the rest: 
gPresD$Sex <- kernowResults$Sex[match(gPresD$rated_ID, kernowResults$ID)]
gPresD$nominated <- kernowResults$Nominated[match(gPresD$rated_ID, kernowResults$ID)]
gPresD$initInf <- kernowResults$initial_influential[match(gPresD$rated_ID, kernowResults$ID)]

###### problem solving for Alice: ####
#increase length of every column to 101 based on run.number and fill the rest with NAs
library(data.table)
alice2<- as.data.table(alice)[, lapply(.SD, `length<-`, 101), by = run.number]

#turn NAs into whatever the previous entry was:
# na.locf:
library(zoo)
alice3 <- na.locf(alice2)

#base R version of this:
# from: https://stackoverflow.com/questions/10554741/fill-in-data-frame-with-values-from-rows-above 
f2 <- function(x) {
  for(i in seq_along(x)[-1]) if(is.na(x[i])) x[i] <- x[i-1]
  x
}

###### Plots: #####

#my spaceship plot:

limits <- aes(ymax = d.predNew$PI.U, ymin = d.predNew$PI.L)
predPlot <- ggplot(data = d.predNew, aes(Condition, means, shape = Sex))
predPlot + geom_point(data = d.predNew, stat="identity", position = position_dodge(width=0.3), size = 2.8) + 
  geom_errorbar(limits, width = 0.08, position = position_dodge(width=0.3)) +
  geom_hline(aes(yintercept=0.5), linetype="dashed", show.legend=FALSE) + 
  theme_bw() + theme(text = element_text(size=12), axis.title.x=element_blank(), axis.title.y=element_text(margin=margin(0,12,0,0))) + 
  ylab("Proportion Chose Social Source") +
  scale_y_continuous(limits=c(0,1), expand = c(0,0)) +
  scale_x_discrete(limits=c("Control", "Social Risky","Asocial Risky")) 

#raw spaceship plot:

rawPlot <- ggplot(myData, aes(Condition, Choice, shape = Sex)) +
  stat_summary(fun.y=mean, position= position_dodge(0.3), geom = "point", size = 2.8) +
  stat_summary(fun.data = mean_cl_normal, position = position_dodge(0.3), geom = "errorbar", width = 0.08) +
  geom_hline(aes(yintercept=0.5), linetype="dashed", show.legend=FALSE) +
  theme_bw() + theme(text = element_text(size=12), axis.title.x=element_blank(), axis.title.y=element_text(margin=margin(0,12,0,0))) + 
  ylab("Proportion Chose Social Information") +
  scale_x_discrete(limits = c("Control", "Social Risky", "Asocial Risky")) +
  scale_y_continuous(limits=c(0,1))
rawPlot

# phd personality plot (density)

personalityPlot <- ggplot(myPData, aes(myPData$personalityScore, fill = Gender)) 
personalityPlot + scale_fill_grey(start = 0.1, end = 0.9) + geom_density(alpha = 0.2) + theme_bw() + 
  theme(text = element_text(size=12), axis.title.y=element_text(margin=margin(0,12,0,0))) +
  scale_y_continuous(limits=c(0,0.09), expand = c(0,0)) +
  scale_x_continuous(limits=c(0,60), expand= c(0,0)) +
  xlab("\nRisky impulsivity Score") + ylab("Density") 


# my raw counts plot, domain-specific prestige

ggplot(data = round2) + 
  geom_bar(mapping = aes(x = condition, fill = info_chosen)) +
  labs(fill = "Participant Choice") +
  xlab("Conditions") + ylab("Total times chosen") +
  theme_bw() +
  theme(text = element_text(size=16), axis.text = element_text(size=14)) + 
  scale_fill_manual(labels = c("Random Cue", "Domain-general prestige","Cross-domain prestige","Domain-specific prestige"), values=cbbPalette)

# facet confidence plot

ggplot(data = confData, aes(x= Confidence)) + 
  geom_bar(stat="count",fill="aquamarine4") +
  facet_wrap(afterGame ~ higherEdu) + 
  xlab("Confidence rating") + ylab("No. of Participants") +
  theme_bw() +
  theme(strip.background =element_rect(fill="aquamarine4")) +
  theme(strip.text = element_text(colour = 'yellow3', size=12))


# Exploratory plot, domain-specific prestige paper (variation individ prestige)

presPlot2 + theme_bw() + 
  geom_vline(aes(xintercept=100), linetype="dashed", show.legend=FALSE) + 
  geom_vline(aes(xintercept=200), linetype="dashed", show.legend=FALSE) +
  geom_vline(aes(xintercept=300), linetype="dashed", show.legend=FALSE) +
  annotate("text", x=50, y=80, label=paste("Condition\nC")) +
  annotate("text", x=150, y=80, label=paste("Condition\nA")) +
  annotate("text", x=250, y=80, label=paste("Condition\nB")) +
  annotate("text", x=350, y=80, label=paste("Condition\nD")) +
  theme(text = element_text(size=12), axis.title.y=element_text(margin=margin(0,12,0,0)), plot.title = element_text(hjust=0.5)) +
  labs(color = "Group ID") +
  xlab("Participant ID") + ylab("Final Prestige Score")

#Alex's pictionary plot: 
pictionary.success.plot <- ggplot(data = pictionary.data, aes(x = player, y = success)) +
  stat_summary(aes(group = 1), fun.y = mean, geom = "line", size = 1) + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2, size = 1) + 
  stat_summary(aes(group = 1), fun.y = mean, geom = "point", size = 2) + 
  theme_classic(base_size = 14) + labs(y = "probability of success") + 
  scale_y_continuous(limits = c(0,1)) + scale_x_continuous(breaks = seq(0,7,by=1))
pictionary.success.plot

#Alex's transmission chain plot: 
tchain.plot <- ggplot(tchain.data, aes(generation, wordcount, colour = material)) + 
  stat_summary(fun.y = mean, geom = "line", size = 1, aes(group = material, colour = material)) + 
  stat_summary(fun.y = mean, geom = "point", size = 3, aes(shape = material)) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2, size = 1) + 
  theme_classic(base_size = 14) + labs(x = "generation", y = "total wordcount") + 
  scale_y_continuous(limits = c(0,NA))

#plot only round 25:
plot(scaleScore ~ ObserveCount, tp[tp$rnd==25,])

###### ordinal prestige plot from rethinking made for our Kernow data: #####
# Initial influence plot (rainbows):
post <- extract.samples(pMFull)
kInit <- 0:1 #values of influence
color_list <- c("red","orange","yellow","green","blue","violet")
plot(1, 1, type = "n", xlab = "Initially influential", ylab = "Cumulative Probability", xlim = c(0,1), ylim = c(0,1), xaxp = c(0,1,1), yaxp = c(0,1,2), main = "Prestige Ratings")
pInit_means <- matrix(0, nrow = 2, ncol = 6) 
for( s in 1:100) {
  p <- as.data.frame(post)[s,]
  ak <- as.numeric(p[1:6])
  phi <- p$bIn*kInit
  pInit <- pordlogit( 1:6, a=ak, phi=phi)
  pInit_means<- pInit_means + pInit
  for ( i in 1:6)
    lines( kInit, pInit[,i], col=col.alpha(color_list[i], alpha = 0.05))
}
# add thick lines for means
for (i in 1:6)
  lines( kInit, pInit_means[,i]/101, col = color_list[i], lwd = 3)
# add labels for numbers
text(0.2, 0.01, labels = "1")
text(0.3, 0.02, labels = "2")
text(0.4, 0.03, labels = "3")
text(0.5, 0.08, labels = "4")
text(0.55, 0.2, labels = "5")
text(0.6, 0.5, labels = "6")
text(0.7, 0.9, labels = "7")

###### Models - simple multilevel binomial in Rethinking package: #####

#turn the dataframe (scoreChoice) into a neat dataframe for stan to read
scoreChoice <- scoreChoice[,c("pptIndex","copied_successful","groupIndex")]
scoreChoice<- as.data.frame(scoreChoice)

#the following model is a binomial (copied_successful = 1,0) with an intercept (a), and varying intercepts for participant and group.
model1 <- map2stan(
  alist(
    copied_successful ~ dbinom(1, p),
    logit(p) <- a + a_p[pptIndex]*sigma_p + a_g[groupIndex]*sigma_g,
    a ~ dnorm(0,4),
    a_p[pptIndex] ~ dnorm(0,1),
    a_g[groupIndex] ~ dnorm(0,1),
    sigma_p ~ dcauchy(0,1),
    sigma_g ~ dcauchy(0,1)
  ),
  data=scoreChoice, constraints=list(sigma_p="lower=0", sigma_g="lower=0"), 
  warmup=1000, iter=4000, chains=3, cores=3)


