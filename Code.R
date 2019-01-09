#Importing Dataset & packages
library(dplyr)
library(tidyr)
library(ggplot2)

#Examining the Data
View(brfss2013)

#Question 1: Do people sleep at least 7hrs have better physical health condition?
Sleeptime <- brfss2013%>%
  mutate(sleeptime = ifelse(sleptim1 >=7, "over 7hr","below 7hr"))%>%
  select(physhlth, sleeptime)
Sleeptime <- na.omit(Sleeptime)

ggplot(aes(x=physhlth, fill=sleeptime), data=Sleeptime)+
  geom_histogram(bins=10, binwidth=10, position = position_dodge()) +ggtitle("Physical Health vs. Sleep time")+
  scale_y_continuous(labels = comma)+
  scale_x_continuous(name = "Number of Days Physical Health Not Good")+
  coord_cartesian(xlim = c(-5, 35)) 

by(Sleeptime$sleeptime, Sleeptime$physhlth, summary)

#Question 2: Do sleep status affect Usual Activities in Past 30 Days?
 DiffConcentrate<- brfss2013%>%
   mutate(sleeptime = ifelse(sleptim1 >=7, "over 7hr","below 7hr"))%>%
   select(sleeptime, painact2)
DiffConcentrate <- na.omit(DiffConcentrate)

ggplot(aes(x=painact2, fill=sleeptime), data=DiffConcentrate)+
  geom_histogram(bins=10, binwidth=10, position = position_dodge()) +ggtitle("Days of Hard to Do Usual Activities vs. Sleep time")+
  scale_x_continuous(name = "Number of Days Hard To Do Usual Activities")+
  coord_cartesian(xlim = c(-5, 35)) 

by(DiffConcentrate$sleeptime, DiffConcentrate$painact2, summary)

#Question 3: How income level correlates with medical cost & is Medicare helping lower income people?
plot(brfss2013$income2, brfss2013$medscost, xlab="Income Level", ylab="Could Not Get Medicine Due To Cost", main="Income Level vs. Cost to Medicine")
by(brfss2013$medscost, brfss2013$income2, summary)

plot(brfss2013$income2, brfss2013$medicare, xlab="Income Level",ylab="Whether Have Medicare or not", main="Income Level vs. Medicare")
by(brfss2013$medicare, brfss2013$income2, summary)

plot(brfss2013$diffdres, brfss2013$medicare, xlab="Have Difficulty Dressing Or Bathing",ylab="Whether Have Medicare or not", main="Medicare vs. Difficulty Dressing Or Bathing")
by( brfss2013$medicare,brfss2013$diffdres,summary)
