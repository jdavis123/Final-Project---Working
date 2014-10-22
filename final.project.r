################################################# 
# Final Project
# Jeremy Thames #
# Predicting NCAA Tournament Entries #
# 9/24/14 #
################################################# 

library(ggplot2)
library(plyr)
library(scales)
library(AUC)
library(caret)


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

#Do some quartile tests to see which of these variables have predictive power

#2013 Season
#Win Loss %
ncaa2013$wl.q<-cut(ncaa2013$W.L.,
            breaks=quantile(ncaa2013$W.L.,probs=seq(0,1,by=.25)),
            include.lowest=TRUE)

rpi.wl.2013<-tapply(ncaa2013$RPI,ncaa2013$wl.q,mean)

tab.wl.q.2013<-aggregate(ncaa2013$RPI,by=list(ncaa2013$wl.q),FUN="mean")

tab.wl.q.2013
#Win Loss % Has a lot of Predictive Power on RPI

#Now let's test Strength of Schedule
ncaa2013$pp100.q<-cut(ncaa2013$SOS,
                   breaks=quantile(ncaa2013$SOS,probs=seq(0,1,by=.25)),
                   include.lowest=TRUE)

rpi.sos.2013<-tapply(ncaa2013$RPI,ncaa2013$pp100.q,mean)

tab.pp100.q.2013<-aggregate(ncaa2013$RPI,by=list(ncaa2013$sos.q),FUN="mean")

tab.sos.q.2013
#Strength of Schedule Has Predictive Power Too

#Let's Move on to Linear Regression
#We'll Need This

rmse <- function(error)
{
  sqrt(mean(error^2))
}

#Using only RPI
#2013
linear.rpi.2013<-lm(Tournament.~RPI,data=ncaa2013) #outcome on left, predictor on right 

summary(linear.rpi.2013)

g.rpi.2013<-ggplot(ncaa2013,aes(x=RPI,y=Tournament.,group=1))+
  geom_jitter()

g.rpi.2013

pred.rpi.2013<-predict(linear.rpi.2013)

rmse(ncaa2013$Tournament.-pred.rpi.2013)

#2012
linear.rpi.2012<-lm(Tournament.~RPI,data=ncaa2012) #outcome on left, predictor on right 

summary(linear.rpi.2012)

g.rpi.2012<-ggplot(ncaa2012,aes(x=RPI,y=Tournament.,group=1))+
  geom_jitter()

g.rpi.2012

pred.rpi.2012<-predict(linear.rpi.2012)

rmse(ncaa2012$Tournament.-pred.rpi.2012)

#2011
linear.rpi.2011<-lm(Tournament.~RPI,data=ncaa2011) 

summary(linear.rpi.2011)

g.rpi.2011<-ggplot(ncaa2011,aes(x=RPI,y=Tournament.,group=1))+
  geom_jitter()

g.rpi.2011

pred.rpi.2011<-predict(linear.rpi.2011)

rmse(ncaa2011$Tournament.-pred.rpi.2011)

#Add in Other Variables
lin.mod.2013<-lm(Tournament.~ RPI+
                              SOS+
                              Pace+
                              PP100.Poss.+
                              TS.+
                              TRB.+
                              AST.+
                              STL.+
                              TOV.
                              ,data=ncaa2013) 

summary(lin.mod.2013)

pred.lin.2013<-predict(lin.mod.2013)

rmse(ncaa2013$Tournament.-pred.lin.2013)


#Logistical Regression Will be a Better Fit
log.mod.2013<-glm(Tournament.~ RPI+
                               SOS+
                               Pace+
                               PP100.Poss.+
                               TS.+
                               TRB.+
                               AST.+
                               STL.+
                               TOV.,
               data=ncaa2013,
               y=TRUE)

summary(log.mod.2013)

log.pred.2013<-predict(log.mod.2013,type="response")

log.roc.2013<-roc(log.pred.2013,as.factor(log.mod.2013$y))

auc(log.roc.2013)

plot(log.roc.2013)

#This is Pretty Compelling

#Let's Test this model on 2012 and 2011







