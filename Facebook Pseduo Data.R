getwd()
list.files()
pf <- read.csv("pseudo_facebook.tsv", sep ='\t')
library(ggplot2)

names(pf)

qplot(x = dob_day, data = pf) +
  scale_x_discrete(count = 1:31) +
  facet_wrap(~dob_month, ncol = 3)

qplot(x = friend_count, data = subset(pf, !is.na(gender)), xlim = c(0, 1000), binwidth = 10)

qplot(x = friend_count, data = subset(pf, !is.na(gender)), xlim = c(0, 1000), binwidth = 10) +
  scale_x_continuous(limits= c(0, 1000), breaks = seq(0,1000, 50))+
  facet_wrap(~gender)
table(pf$gender)
by(pf$friend_count, pf$gender, summary)


qplot(x = tenure/365, data = pf,
      xlab = 'Number of years using Facebook',
      ylab = 'Y Axis',
      color = I('black'), fill = I('#099DD9'), binwidth = .25)

qplot(x= age, data = pf, 
      xlab = 'Age of user', 
      ylab = 'Count of Users', 
      color = I('black'), fill = I('#099DD9'),
      binwidth =1)

qplot(x= log10(friend_count + 1), data = pf, 
      xlab = 'Log of Friendcount', 
      ylab = 'Count of Users', 
      color = I('black'), fill = I('#099DD9'))

qplot(x= friend_count, y = ..count../sum(..count..), 
      data = subset(pf, !is.na(gender)), 
      binwidth =10, geom = 'freqpoly', color = gender)+
  scale_x_continuous(lim = c(0,1000), breaks = seq(0,1000, 50)) 

summary(pf$www_likes)
qplot(x= log10(www_likes), y = ..count../sum(..count..), 
      data = subset(pf, !is.na(gender)),
      geom = 'freqpoly', color = gender)+
  scale_x_log10()+
  scale_x_continuous() 

table(pf$www_likes)
by(pf$friend_count, pf$gender, summary)
by(pf$www_likes, pf$gender, sum)

qplot(x = gender, y = friend_count, 
      data = subset(pf, !is.na(gender)), geom= 'boxplot') +
  coord_cartesian(ylim = c(0,250))

by(pf$friendships_initiated, pf$gender, sum)


Output2 <- summary(pf$mobile_likes>0)

mobile_check_in <- NA
pf$mobile_check_in <- ifelse(pf$mobile_likes> 0, 1, 0)
pf$mobile_check_in <- factor(pf$mobile_check_in)
summary(pf$mobile_check_in)

pf$mobile_check_in$1 / pf$mobile_check_in$0

63947/(35056+63947)
















