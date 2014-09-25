################################################# 
# Final Project
# Jeremy Thames #
# Predicting NCAA Tournament Entries #
# 9/24/14 #
################################################# 

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

