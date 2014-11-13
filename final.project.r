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

ncaa2012<-read.table("C:/Users/Jeremy/Documents/GitHub/Final-Project---Working/2012.stats.csv",
                     sep=",",
                     header=TRUE,
                     as.is=TRUE)

ncaa2012<-ncaa2012[,-2]
ncaa2012<-ncaa2012[,-2]
ncaa2012<-ncaa2012[,-2]

ncaa2011<-read.table("C:/Users/Jeremy/Documents/GitHub/Final-Project---Working/2011.stats.csv",
                     sep=",",
                     header=TRUE,
                     as.is=TRUE)

ncaa2011<-ncaa2011[,-2]
ncaa2011<-ncaa2011[,-2]
ncaa2011<-ncaa2011[,-2]

#Logistical Regression
log.mod.2013<-glm(Tournament.~ RPI:SOS+ #This interaction is good
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
log.roc.new<-roc(pred.new,as.factor(log.mod.2013$y))
auc(log.roc.new)

pred.new1<-predict(log.mod.2013,newdata=ncaa2011, type="response")
log.roc.new1<-roc(pred.new1,as.factor(log.mod.2013$y))
auc(log.roc.new1)

auc(log.roc.2013)
auc(log.roc.new)
auc(log.roc.new1)
#Not bad but we're still overfitting

#Let's try Bootstrapping using a combined data set
#first combine the data sets

ncaa<-rbind(ncaa2013,ncaa2012,ncaa2011)

#Put a model in using ncaa data set
log.mod<-glm(Tournament.~ RPI:SOS+ 
                          Pace+
                          PP100.Poss.+ 
                          AST.+
                          TOV., 
                          data=ncaa,
                          y=TRUE)

summary(log.mod)

#Now let's run the bootstrap samples
thames.boot<-Boot(log.mod,R=10000)

summary(thames.boot,high.moments=F)

#Here are our new confidence intervals
confint(thames.boot,level=.9,type="norm")

#Rig it like this to find the midpoint
confint(thames.boot,level=.000000001,type="norm")

#And our coefficients are
#intercept = -2.397788
#Pace = -3.900091e-3
#PP100.Poss. = 2.628452e-2
#AST. = 3.07147e-3
#TOV. = 4.856387e-4
#RPI:SOS = 3.182813e-5

#Let's load those coefficients into the model
log.mod$coefficients
log.mod$coefficients$'(Intercept)'<- -2.397788
log.mod$coefficients$Pace<- -3.900091e-3
log.mod$coefficients$PP100.Poss.<- 2.628452e-2
log.mod$coefficients$AST.<- 3.07147e-3
log.mod$coefficients$TOV.<- 4.856387e-4
log.mod$coefficients$'RPI:SOS' <- 3.182813e-5

log.mod$coefficients

#Let's use these coefficients on the yearly datasets and check AUCs
predict.2013<-predict(log.mod,newdata=ncaa2013, type="response")
predict.roc.2013<-roc(predict.2013,as.factor(log.mod$y))
auc(predict.roc.2013)

#^Here's what doesnt't work



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
