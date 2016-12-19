library(dplyr)
library(ggplot2)
activity<-read.csv("activity.csv")

##Remove NA's from data
dat<-activity[complete.cases(activity),]

##group by days
groupDay<-group_by(dat,date)
##calculate total steps per day
stepsPerDay<-summarise(groupDay,total=sum(steps))

##calculate mean and median of total steps
meanSteps<-mean(stepsPerDay$total)
medianSteps<-median(stepsPerDay$total)

##plot histogram of total steps per day
g<-ggplot(stepsPerDay,aes(x=total))+geom_histogram(bins=10,aes(fill=..count..),origin=0)+
  labs(title="Histogram of steps per day")+ ylab("Number of days")+xlab("Total Steps")+
geom_vline(xintercept = medianSteps,linetype="longdash")
print(g)


##group by interval
groupInterval<-group_by(dat,interval)
##calculae steps per interval
stepsperInterval<-summarise(groupInterval,avg=mean(steps))
maxavg=max(stepsperInterval$avg)
maxinterval=subset(stepsperInterval,avg==maxavg)$interval

g1<-ggplot(stepsperInterval,aes(x=interval,y=avg))+geom_line() +
  labs(title="Maximum number of steps are present in 835th Interval")+ ylab("Average steps")+xlab("Intervals")+
  geom_text(aes(label=round(avg,2)),data=subset(stepsperInterval,avg==maxavg),colour = "blue", fontface = "bold",size=3) +
  geom_vline(xintercept =maxinterval ,linetype="longdash") +
  annotate("text", label = paste(maxinterval,"th interval"), x = maxinterval+10, y = 1, size = 3, colour = "red",fontface = "bold")


print(g1)