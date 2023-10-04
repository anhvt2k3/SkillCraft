#1. Import data:
SkillCraft <- read.csv("~/Study/221/Probability_and_Statistics/Assignment/Data/SkillCraft.csv")

#2. Data cleaning:
sum(is.na(SkillCraft))
SkillCraftCleaned<-subset(SkillCraft,select=-c(GameID, WorkersMade, UniqueUnitsMade, ComplexUnitsMade, ComplexAbilitiesUsed))

#3. Data visualization:

#a) Discriptive statistics:
Mean<-apply(SkillCraftCleaned[,c(2:15)],2,mean)
StandardDeviation<-apply(SkillCraftCleaned[,c(2:15)],2,sd)
Q1<-apply(SkillCraftCleaned[,c(2:15)],2,quantile,probs=0.25)
Median<-apply(SkillCraftCleaned[,c(2:15)],2,quantile,probs=0.5)
Q3<-apply(SkillCraftCleaned[,c(2:15)],2,quantile,probs=0.75)
Max<-apply(SkillCraftCleaned[,c(2:15)],2,max)
Min<-apply(SkillCraftCleaned[,c(2:15)],2,min)
DiscriptiveStatistics<-data.frame(Mean,StandardDeviation,Q1,Median,Q3,Max,Min)

#b) Plotting graph:
hist(SkillCraft$LeagueIndex,xlab="Ranking",ylab="Number",main="Histogram of League Index",label=T,xlim=c(0,8),ylim= c(0,1000),breaks=c(0,1,2,3,4,5,6,7,8),col=2)
par(mfrow=c(2,3))
plot(SkillCraftCleaned$Age,SkillCraft$LeagueIndex,xlab='Age',ylab='League Index',col=4)
plot(SkillCraftCleaned$HoursPerWeek,SkillCraftCleaned$LeagueIndex,xlab='Hours Per Week',ylab='League Index',col=4)
plot(SkillCraftCleaned$TotalHours,SkillCraftCleaned$LeagueIndex,xlab='Total Hours',ylab='League Index', xlim=c(0,10000),col=4)
plot(SkillCraftCleaned$APM,SkillCraftCleaned$LeagueIndex,xlab='APM',ylab='League Index',col=4)
plot(SkillCraftCleaned$SelectByHotkeys,SkillCraftCleaned$LeagueIndex,xlab='Select By Hotkeys',ylab='League Index',col=4)
plot(SkillCraftCleaned$AssignToHotkeys,SkillCraftCleaned$LeagueIndex,xlab='Assign To Hotkeys',ylab='League Index',col=4)
plot(SkillCraftCleaned$UniqueHotkeys,SkillCraftCleaned$LeagueIndex,xlab='Unique Hotkeys',ylab='League Index',col=4)
plot(SkillCraftCleaned$MinimapAttacks,SkillCraftCleaned$LeagueIndex,xlab='Minimap Attacks',ylab='League Index',col=4)
plot(SkillCraftCleaned$MinimapRightClicks,SkillCraftCleaned$LeagueIndex,xlab='Minimap Right Clicks',ylab='League Index',col=4)
plot(SkillCraftCleaned$NumberOfPACs,SkillCraftCleaned$LeagueIndex,xlab='Number Of PACs',ylab='League Index',col=4)
plot(SkillCraftCleaned$GapBetweenPACs,SkillCraftCleaned$LeagueIndex,xlab='Gap Between PACs',ylab='League Index',col=4)
plot(SkillCraftCleaned$ActionLatency,SkillCraftCleaned$LeagueIndex,xlab='Action Latency',ylab='League Index',col=4)
plot(SkillCraftCleaned$ActionsInPAC,SkillCraftCleaned$LeagueIndex,xlab='Actions In PAC',ylab='League Index',col=4)
plot(SkillCraftCleaned$TotalMapExplored,SkillCraftCleaned$LeagueIndex,xlab='Total Map Explored',ylab='LeagueIndex',col=4)

#4.Linear regression model:

#a) Split data to train data and test data:
sample <- sample(c(TRUE, FALSE), nrow(SkillCraft), replace=TRUE, prob=c(0.8,0.2))
train  <- SkillCraftCleaned[sample, ]
test   <- SkillCraftCleaned[!sample, ]

#b) Correlation lost:
correlate<-cor(train[,c(  
  'HoursPerWeek'
  ,  'Age'
  ,'TotalHours'
  ,'SelectByHotkeys'
  ,'UniqueHotkeys'
  ,'MinimapRightClicks'
  ,'ActionsInPAC'
  ,'APM'
  ,'AssignToHotkeys'
  ,'MinimapAttacks'
  ,'NumberOfPACs'
  ,'TotalMapExplored'
  ,'GapBetweenPACs'
  ,'ActionLatency'
)])
corr <- sort(correlate, decreasing = TRUE)
corr <- corr[which(corr != 1)]
minc <- corr[which(corr <= -0.8, arr.ind = T)] # take out the under - 0.8
maxc <- corr[which(corr >= 0.8, arr.ind = T)] # and over 0.8
which(correlate == minc, arr.ind = T)

#c) Building model:
model1<-lm(LeagueIndex~
             Age
           +HoursPerWeek
           +TotalHours
           +SelectByHotkeys
           +UniqueHotkeys
           +MinimapRightClicks
           +ActionsInPAC
           +APM
           +AssignToHotkeys
           +MinimapAttacks
           +NumberOfPACs
           +TotalMapExplored
           +GapBetweenPACs
           +ActionLatency
           ,data=train)
summary(model1)
model2<-lm(LeagueIndex~
             +HoursPerWeek
           +SelectByHotkeys
           +UniqueHotkeys
           +ActionsInPAC
           +AssignToHotkeys
           +MinimapAttacks
           +NumberOfPACs
           +TotalMapExplored
           +GapBetweenPACs
           +ActionLatency
           ,data=train)
summary(model2)
model3<-lm(LeagueIndex~
             +HoursPerWeek
           +SelectByHotkeys
           +UniqueHotkeys
           +ActionsInPAC
           +AssignToHotkeys
           +MinimapAttacks
           +TotalMapExplored
           +GapBetweenPACs
           +ActionLatency
           ,data=train)
summary(model3)
model3<-lm(LeagueIndex~
             +HoursPerWeek
           +SelectByHotkeys
           +UniqueHotkeys
           +AssignToHotkeys
           +MinimapAttacks
           +GapBetweenPACs
           +ActionLatency
           ,data=train)
summary(model3)
anova(model1,model3)

#d) Predictions:
predicts <- predict(model3, newdata = test)
predicts <- round(predicts)
actuals <- test$LeagueIndex
evaluate <- data.frame(actuals,predicts,predicts-actuals)
summary(evaluate)
