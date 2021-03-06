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

ncaa2013<-read.table("/Users/joshdavis1001/Documents/GitHub/Final-Project---Working/2013.stats.csv",
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


ncaa2012<-read.table("/Users/joshdavis1001/Documents/GitHub/Final-Project---Working/2012.stats.csv",
                     sep=",",
                     header=TRUE,
                     as.is=TRUE)

ncaa2012<-ncaa2012[,-2]
ncaa2012<-ncaa2012[,-2]
ncaa2012<-ncaa2012[,-2]

ncaa2012$flag<-0
ncaa2012$flag[ncaa2012$Tournament.==1 & ncaa2012$RPI<40]<-1
ncaa2012$flag[ncaa2012$Tournament.==1 & ncaa2012$auto.bid==0]<-1


ncaa2011<-read.table("/Users/joshdavis1001/Documents/GitHub/Final-Project---Working/2011.stats.csv",
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
log.mod.2013<-glm(flag~ RPI+ 
                        SOS+
                        PP100.Poss.,
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
                   PP100.Poss., 
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

ncaa2010<-read.table("/Users/joshdavis1001/Documents/GitHub/Final-Project---Working/2010.stats.csv",
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

#AUC = .975

plot(predict.roc.2013,main="2013\nAUC = .99")
plot(predict.roc.2012,main="2012\nAUC = .99")
plot(predict.roc.2011,main="2011\nAUC = .97")
plot(predict.roc.2010,main="2010\nAUC = .98")


#Put the probability of tournement entry into the data sets as a variable
ncaa2013$probability<-predict.2013
ncaa2012$probability<-predict.2012
ncaa2011$probability<-predict.2011
ncaa2010$probability<-predict.2010

#Plot RPI vs. Projected Probability for each year 
#2013
g2013<-ggplot(data=ncaa2013,
           aes(x=RPI,y=probability))

g2013<-g2013+geom_point() + ggtitle("2013 RPI vs. Projected Probability")
g2013

#2012
g2012<-ggplot(data=ncaa2012,
              aes(x=RPI,y=probability))

g2012<-g2012+geom_point() + ggtitle("2012 RPI vs. Projected Probability")
g2012

#2011
g2011<-ggplot(data=ncaa2011,
              aes(x=RPI,y=probability))

g2011<-g2011+geom_point() + ggtitle("2011 RPI vs. Projected Probability")
g2011

#2010
g2010<-ggplot(data=ncaa2010,
              aes(x=RPI,y=probability))

g2010<-g2010+geom_point() + ggtitle("2010 RPI vs. Projected Probability")
g2010


#Let's order these based on the probability
ncaa2013<-ncaa2013[order(-ncaa2013$probability),]
ncaa2012<-ncaa2012[order(-ncaa2012$probability),]
ncaa2011<-ncaa2011[order(-ncaa2011$probability),]
ncaa2010<-ncaa2010[order(-ncaa2010$probability),]

#Now we have a rank order of likelihood
#Different years have different amounts of spots not stolen by auto bids
#Luckily, you can use our trusty flag to figure out how many spots are legit
#This will show you how many teams should be considered "in" in each year

sum(ncaa2013$flag)
sum(ncaa2012$flag)
sum(ncaa2011$flag)
sum(ncaa2010$flag)

in.2013<-ncaa2013[1:47,c(1,16,18,19)]
in.2012<-ncaa2012[1:48,c(1,16,18,19)]
in.2011<-ncaa2011[1:50,c(1,16,18,19)]
in.2010<-ncaa2010[1:37,c(1,16,18,19)]

out.2013<-ncaa2013[48:346,c(1,16,18,19)]
out.2012<-ncaa2012[49:344,c(1,16,18,19)]
out.2011<-ncaa2011[51:341,c(1,16,18,19)]
out.2010<-ncaa2010[38:345,c(1,16,18,19)]

last.four.in.2013<-ncaa2013[44:47,c(1,16,19)]
last.four.in.2012<-ncaa2012[45:48,c(1,16,19)]
last.four.in.2011<-ncaa2011[47:50,c(1,16,19)]
last.four.in.2010<-ncaa2010[34:37,c(1,16,19)]

first.four.out.2013<-ncaa2013[48:51,c(1,16,19)]
first.four.out.2012<-ncaa2012[49:52,c(1,16,19)]
first.four.out.2011<-ncaa2011[51:54,c(1,16,19)]
first.four.out.2010<-ncaa2010[38:41,c(1,16,19)]

next.four.out.2013<-ncaa2013[52:55,c(1,16,19)]
next.four.out.2012<-ncaa2012[53:56,c(1,16,19)]
next.four.out.2011<-ncaa2011[55:58,c(1,16,19)]
next.four.out.2010<-ncaa2010[42:45,c(1,16,19)]

last.four.in.2013
first.four.out.2013
next.four.out.2013

#Subset to only teams that didn't get in

snub.2013<-ncaa2013[ncaa2013$Tournament.==0,]
snub.2013<-snub.2013[,c(1,15,19)]

snub.2012<-ncaa2012[ncaa2012$Tournament.==0,]
snub.2012<-snub.2012[,c(1,15,19)]

snub.2011<-ncaa2011[ncaa2011$Tournament.==0,]
snub.2011<-snub.2011[,c(1,15,19)]

snub.2010<-ncaa2010[ncaa2010$Tournament.==0,]
snub.2010<-snub.2010[,c(1,15,19)]


#Now let's Compare Flag vs. Predicted Probability
#first create predict in variable

ncaa2013$hit<-0
ncaa2013$hit[ncaa2013$probability>.4428]<-1

ncaa2012$hit<-0
ncaa2012$hit[ncaa2012$probability>.4143]<-1

ncaa2011$hit<-0
ncaa2011$hit[ncaa2011$probability>.3908]<-1

ncaa2010$hit<-0
ncaa2010$hit[ncaa2010$probability>.4467]<-1

#2013
g2013<-ggplot(data=ncaa2013, 
              aes(x=hit,y=flag))

g2013<-g2013 + ggtitle("2013 Deserved Entry vs. Predicted Entry") + ylim(-1,2) +xlim(0,1) + geom_point(position = position_jitter(w = 0.2, h = 0.2))+xlab("Predicted")+ylab("Deserved")
g2013

#2012
g2012<-ggplot(data=ncaa2012,
              aes(x=hit,y=flag))

g2012<-g2012+geom_point(position = position_jitter(w = 0.2, h = 0.2)) + ggtitle("2012 Deserved Entry vs. Predicted Entry")+ ylim(-1,2) +xlim(0,1)+xlab("Predicted")+ylab("Deserved")
g2012

#2011
g2011<-ggplot(data=ncaa2011,
              aes(x=hit,y=flag))

g2011<-g2011+geom_point(position = position_jitter(w = 0.2, h = 0.2)) + ggtitle("2011 Deserved Entry vs. Predicted Entry")+ ylim(-1,2) +xlim(0,1)+xlab("Predicted")+ylab("Deserved")
g2011

#2010
g2010<-ggplot(data=ncaa2010,
              aes(x=hit,y=flag))

g2010<-g2010+geom_point(position = position_jitter(w = 0.2, h = 0.2)) + ggtitle("2010 Deserved Entry vs. Predicted Entry")+ ylim(-1,2) +xlim(0,1)+xlab("Predicted")+ylab("Deserved")
g2010



#Let's start making some graphics that tell us how well we did


#2013

in.2013$predict<-"Predict IN"
out.2013$predict<-"Predict OUT"
final.2013<-rbind(in.2013,out.2013)
temp.2013<-final.2013[final.2013$flag==1,]
temp.2013$flag1<-"Actual IN"
temp.1.2013<-final.2013[final.2013$flag==0,]
temp.1.2013$flag1<-"Actual OUT"
final.2013<-rbind(temp.2013,temp.1.2013)
remove(temp.2013)
remove(temp.1.2013)
table.2013<-table(final.2013$predict,final.2013$flag1)
p.table.2013<-prop.table(table.2013,margin=1)

#2012

in.2012$predict<-"Predict IN"
out.2012$predict<-"Predict OUT"
final.2012<-rbind(in.2012,out.2012)
temp.2012<-final.2012[final.2012$flag==1,]
temp.2012$flag1<-"Actual IN"
temp.1.2012<-final.2012[final.2012$flag==0,]
temp.1.2012$flag1<-"Actual OUT"
final.2012<-rbind(temp.2012,temp.1.2012)
remove(temp.2012)
remove(temp.1.2012)
table.2012<-table(final.2012$predict,final.2012$flag1)
p.table.2012<-prop.table(table.2012,margin=1)

#2011

in.2011$predict<-"Predict IN"
out.2011$predict<-"Predict OUT"
final.2011<-rbind(in.2011,out.2011)
temp.2011<-final.2011[final.2011$flag==1,]
temp.2011$flag1<-"Actual IN"
temp.1.2011<-final.2011[final.2011$flag==0,]
temp.1.2011$flag1<-"Actual OUT"
final.2011<-rbind(temp.2011,temp.1.2011)
remove(temp.2011)
remove(temp.1.2011)
table.2011<-table(final.2011$predict,final.2011$flag1)
p.table.2011<-prop.table(table.2011,margin=1)

#2010

in.2010$predict<-"Predict IN"
out.2010$predict<-"Predict OUT"
final.2010<-rbind(in.2010,out.2010)
temp.2010<-final.2010[final.2010$flag==1,]
temp.2010$flag1<-"Actual IN"
temp.1.2010<-final.2010[final.2010$flag==0,]
temp.1.2010$flag1<-"Actual OUT"
final.2010<-rbind(temp.2010,temp.1.2010)
remove(temp.2010)
remove(temp.1.2010)
table.2010<-table(final.2010$predict,final.2010$flag1)
p.table.2010<-prop.table(table.2010,margin=1)


heights<-c(.9595,.0405,.9767,.0233,.9531,.0469,.9362,.0638)
mydata<-matrix(heights,
               ncol=2,
               byrow=T,
               dimnames=list(c("2013","2012","2011","2010"),
                             c("Correct","Incorrect")))
               
colors<-c("green","red")

barplot(t(mydata),
        beside=T,
        horiz=F,
        col=colors,
        ylim=c(0,1.4),
        axes=F,
        )
axis(2,at = c(0,.2,.4,.6,.8,1),labels = c("0%","20%","40%","60%","80%","100%"),las=T)
title(main=("Model Performance by Year"),xlab="Year")
legend("topright",colnames(mydata),fill=colors,bty="n")



