################################################# 
# Final Project
# Jeremy Thames & Josh Davis#
# Predicting NCAA Tournament Entries #
# 9/24/14 #
################################################# 

library(ggplot2)
library(plyr)
library(scales)
library(AUC)
library(caret)
library(car)
library(boot)

#Pull The '13-'14,  '12-'13, & '11-'12 Data From Our CSV File

ncaa2013<-read.table("C:/Users/Jeremy/Documents/GitHub/Final-Project---Working/2013.stats.csv",
           sep=",",
           header=TRUE,
           as.is=TRUE)

ncaa2013<-ncaa2013[,-2]
ncaa2013<-ncaa2013[,-2]
ncaa2013<-ncaa2013[,-2]

#Make a new variable that indicates the team made the tournament but not because they won their
#conference tourney
ncaa2013$flag<-0
ncaa2013$flag[ncaa2013$Tournament.==1 & ncaa2013$RPI<40]<-1
ncaa2013$flag[ncaa2013$Tournament.==1 & ncaa2013$auto.bid==0]<-1


ncaa2012<-read.table("C:/Users/Jeremy/Documents/GitHub/Final-Project---Working/2012.stats.csv",
                     sep=",",
                     header=TRUE,
                     as.is=TRUE)

ncaa2012<-ncaa2012[,-2]
ncaa2012<-ncaa2012[,-2]
ncaa2012<-ncaa2012[,-2]

ncaa2012$flag<-0
ncaa2012$flag[ncaa2012$Tournament.==1 & ncaa2012$RPI<40]<-1
ncaa2012$flag[ncaa2012$Tournament.==1 & ncaa2012$auto.bid==0]<-1


ncaa2011<-read.table("C:/Users/Jeremy/Documents/GitHub/Final-Project---Working/2011.stats.csv",
                     sep=",",
                     header=TRUE,
                     as.is=TRUE)

ncaa2011<-ncaa2011[,-2]
ncaa2011<-ncaa2011[,-2]
ncaa2011<-ncaa2011[,-2]

ncaa2011$flag<-0
ncaa2011$flag[ncaa2011$Tournament.==1 & ncaa2011$RPI<40]<-1
ncaa2011$flag[ncaa2011$Tournament.==1 & ncaa2011$auto.bid==0]<-1

#Logistical Regression
log.mod.2013<-glm(flag~ RPI:SOS+ #This interaction is good
                               SOS+
                               Pace+
                               PP100.Poss.+ #This one definitely stays
                               #TS.+ #This one is terrible
                               #TRB.+
                               AST.+
                               #STL.+
                               TOV., #Turnover Margin is good
                               data=ncaa2013,
                               y=TRUE)

summary(log.mod.2013)
log.pred.2013<-predict(log.mod.2013,type="response")
log.roc.2013<-roc(log.pred.2013,as.factor(log.mod.2013$y))
auc(log.roc.2013)
plot(log.roc.2013)

#Let's Test this model on 2012 and 2011

pred.new<-predict(log.mod.2013,newdata=ncaa2012, type="response")
log.roc.new<-roc(pred.new,as.factor(ncaa2012$flag))
auc(log.roc.new)

pred.new1<-predict(log.mod.2013,newdata=ncaa2011, type="response")
log.roc.new1<-roc(pred.new1,as.factor(ncaa2011$flag))
auc(log.roc.new1)

auc(log.roc.2013)
auc(log.roc.new)
auc(log.roc.new1)
#Little Bit of Overfitting

#Let's try Bootstrapping using a combined data set
#first combine the data sets

ncaa<-rbind(ncaa2013,ncaa2012,ncaa2011)

#Put a model in using ncaa data set
log.mod<-glm(flag~ RPI+
                   SOS+ 
                   #Pace+
                   PP100.Poss., 
                   #AST.+
                   #TOV., 
                   data=ncaa,
                   y=TRUE)

summary(log.mod)

#Now let's run the bootstrap samples
thames.boot<-Boot(log.mod,R=10000)

summary(thames.boot,high.moments=F)

#Here are our new confidence intervals
confint(thames.boot,level=.9,type="norm")

#And our coefficients are
#intercept = -0.2051644
#RPI = -0.0011209
#SOS = 0.0199085
#PP100.Poss. = 0.0052917

#Let's load those coefficients into the model
log.mod$coefficients
log.mod$coefficients["(Intercept)"]<- -0.2051644
log.mod$coefficients["RPI"]<- -0.0011209
log.mod$coefficients["SOS"]<- 0.0199085
log.mod$coefficients["PP100.Poss."]<- 0.0052917

log.mod$coefficients

#Let's use these coefficients on the yearly datasets and check AUCs
predict.2013<-predict(log.mod,newdata=ncaa2013, type="response")
predict.roc.2013<-roc(predict.2013,as.factor(ncaa2013$flag))
auc(predict.roc.2013)

predict.2012<-predict(log.mod,newdata=ncaa2012, type="response")
predict.roc.2012<-roc(predict.2012,as.factor(ncaa2012$flag))
auc(predict.roc.2012)

predict.2011<-predict(log.mod,newdata=ncaa2011, type="response")
predict.roc.2011<-roc(predict.2011,as.factor(ncaa2011$flag))
auc(predict.roc.2011)

#This is awesome, we have AUCs of above .969 for all three years
#Now for the real test, let's read in 2010 data and check our model on that

ncaa2010<-read.table("C:/Users/Jeremy/Documents/GitHub/Final-Project---Working/2010.stats.csv",
                     sep=",",
                     header=TRUE,
                     as.is=TRUE)

ncaa2010<-ncaa2010[,-2]
ncaa2010<-ncaa2010[,-2]
ncaa2010<-ncaa2010[,-2]

ncaa2010$flag<-0
ncaa2010$flag[ncaa2010$Tournament.==1 & ncaa2010$RPI<40]<-1
ncaa2010$flag[ncaa2010$Tournament.==1 & ncaa2010$auto.bid==0]<-1


#Here we go
predict.2010<-predict(log.mod,newdata=ncaa2010, type="response")
predict.roc.2010<-roc(predict.2010,as.factor(ncaa2010$flag))
auc(predict.roc.2010)

#AUC = .97

#Put the probability of tournement entry into the data sets as a variable
ncaa2013$probability<-predict.2013
ncaa2012$probability<-predict.2012
ncaa2011$probability<-predict.2011
ncaa2010$probability<-predict.2010

#Let's order these based on the probability
ncaa2013<-ncaa2013[order(-ncaa2013$probability),]
ncaa2012<-ncaa2012[order(-ncaa2012$probability),]
ncaa2011<-ncaa2011[order(-ncaa2011$probability),]
ncaa2010<-ncaa2010[order(-ncaa2010$probability),]

#Now we have a rank order of likelihood
View(ncaa2013)

in.2013<-ncaa2013[1:64,c(1,16,19)]
in.2012<-ncaa2012[1:64,c(1,16,19)]
in.2011<-ncaa2011[1:64,c(1,16,19)]
in.2010<-ncaa2010[1:61,c(1,16,19)]

last.four.in.2013<-ncaa2013[65:68,c(1,16,19)]
last.four.in.2012<-ncaa2012[65:68,c(1,16,19)]
last.four.in.2011<-ncaa2011[65:68,c(1,16,19)]
last.four.in.2010<-ncaa2010[62:65,c(1,16,19)]

first.four.out.2013<-ncaa2013[69:72,c(1,16,19)]
first.four.out.2012<-ncaa2012[69:72,c(1,16,19)]
first.four.out.2011<-ncaa2011[69:72,c(1,16,19)]
first.four.out.2010<-ncaa2010[66:69,c(1,16,19)]

next.four.out.2013<-ncaa2013[73:76,c(1,16,19)]
next.four.out.2012<-ncaa2012[73:76,c(1,16,19)]
next.four.out.2011<-ncaa2011[73:76,c(1,16,19)]
next.four.out.2010<-ncaa2010[70:73,c(1,16,19)]

last.four.in.2013
first.four.out.2013
next.four.out.2013

#let's investigate the teams that we predicted would get in but didn't
#see if there are any articles about them getting snubbed
#also let's check if there are any surprises that got in without being conference champs
#use the automatic bid variable in the 2010 data frame

#Harvard was a huge "snub" in the 2010 season
#let's see what we predicted

#Drexel was a huge snub in the 2011 season

#SMU in 2013
#Kentucky in 2012





#We could show a confusion matrix that tells us how many teams we're correctly predicting
#Random sampling from the combined - bootstrap
#We can go through and show names on the log.mod, then we can reassign coefficients like this
#   log.mod.2013$coefficients$AST.<-10
#maybe we can look at predicting RPI
#what affects RPI can lead to suggestions for coaches
#deliverables: hypothetical bracket, last four in, first four out, next four out
#look at the shape of the probabilities, let's do it with 2010
#start presentation with snubbed teams - who gets in who doesn't
#what does the selection committee use?
