
library(ggplot2)
data(diamonds)
#Below gave the answer to Q1#
length(diamonds$price)
names(diamonds)
summary(diamonds)
#Below gave the answers to Q2, Q3, Q4#
?diamonds

by(diamonds$price, diamonds$color, summary)

#Problem set 3, Part 2) 

qplot(x = price, data = diamonds) +
  scale_x_continuous()

#Part 3 Data represents a longtailed distribution of the data with a median price around 2400 dollars.#

#Part 4
sum(diamonds$price < 500)
sum(diamonds$price < 250)
sum(diamonds$price >= 15000)

#Part 5
qplot(x = price, data = diamonds, binwidth = 10) +
  scale_x_continuous(limits= c(300, 1200), breaks = seq(450,1200, 100))
#Part 6
qplot(x = price, data = diamonds, binwidth = 10) +
  scale_x_continuous(limits= c(300, 1200), breaks = seq(450,1200, 100))+
  facet_wrap(~cut)
#Part 7
by(diamonds$price, diamonds$cut, max)
by(diamonds$price, diamonds$cut, min)
by(diamonds$price, diamonds$cut, median)
#Part 8
qplot(x = price, data = diamonds) + facet_wrap(~cut, scales = "free_y")
?facet_wrap
# Part 9
qplot(y=price/carat, data=diamonds)+
  scale_x_log10()+
  facet_wrap(~cut)
# Part 10
plot(y=price/carat, x=color,  data=diamonds, geom="boxplot", fill=color)+
  coord_cartesian(ylim=c(0,7500))
# Part 11
?diamonds
by(diamonds$price, diamonds$color, summary)
# Part 12
qplot(y=price/carat, x=color,  data=diamonds, geom="boxplot", fill=color)+
  coord_cartesian(ylim=c(0,7500))
#Part 13
qplot(x = carat, data = diamonds, geom = "freqpoly", color = carat, binwidth = .1) +
  scale_x_continuous(limits= c(0,3), breaks = seq(0,3, .1))+
  coord_cartesian(ylim = c(0,2000))

table(diamonds$carat)

# Part 14
#Reading in data realted to the Gapminder dataset for Cellphones per 100 people
gap <- read.csv("Gapminder- Cell phone per 100.csv", sep =',')
names(gap)
names(gap)[1]<- "country"
library(dplyr)
library(tidyr)

#Utilizing Tidyr to manipulate data into a Tidy table
dat <- gather(gap, "year","cell_phone_per_100", 2:48)

#Cleaing out the character X which had marked all of my Year data
dat$year <- gsub('X','',dat$year)

#Plot1 - Plotting a Histogram for the year 2011 for all countries in the dataset
qplot(y = cell_phone_per_100, x = year,  data= subset(dat, year== 2011), 
      geom="boxplot",
      xlab = 'Year 2011',
      ylab = 'Cell phones per 100 people') +
  coord_cartesian(ylim = c(25,175))

# Plot2 - 1st filtering for data in 1990 and 2011 because I couldn't get my | logical operator right. 

dat2011 <- filter(dat, year == 2011)
dat2000 <- filter(dat, year == 2000)

# 2nd Merging the two datasets
data2000_2011 <- full_join(dat2000, dat2011)

# 3rd Plotting the distrubtion of world cell phones per 100 people in 2011 and 1990
qplot(x = log10(cell_phone_per_100), data = data2000_2011,
      geom = 'freqpoly', color = year)


# Part 15-  Used the Provided Facebook data to perform the analysis on DOB Data
qplot(x = dob_day, data = pf) +
  scale_x_discrete(count = 1:31) +
  facet_wrap(~dob_month, ncol = 3)






