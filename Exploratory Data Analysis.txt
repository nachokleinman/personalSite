Exploratory Data Analysis helps us having introuductory idea on how the dataset is like.

###Ch1. Basic calculation: R work well for big number
#Product (i.e., multiplication) of 123,456 and 456,789
123456*456789
log(100, base =10)
#Log to the base 2 of 33554432 vs exp is exponentials
log(33554432, base =2) #equals to
log2(33554432) #but not eqaul to 
log(33554432, base = exp(2))
#The result of line 7 is equal to line 8, but not equal to line 9

#square root
sqrt(81)
log10(1)
log2(2)

#Cosine of 0 Radians. 
acos(0)
#Trigonometric Functions
cos(0)

###To know how many observations a code could generate or the size of a dataset. We use length() function
length(rnorm(n=2021,mean = 10,sd = 5))

###Before data maninuplation, we should have an idea on the class (type) of the value we are questing for.
class(7)
class('seven')
class(2021 != 2020)
class(23 * 37 + 67 * T) #In this case, T(True) is treated as 1, F(False) is 0
class(c("Am I really smart", TRUE, 1000))
c(10,20,30,40) > 15
c(10,20,30,40) > c(15,25,35,45)
c(10,20,30,40) > c(15,25)

```
Now we could clean the environment before we continue.
rm(list=ls())

We create a dataframe as followed: 
day_of_week = c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday')
number_of_smartwatches_sold = c(10,20,15,20,30,80,90)
price_per_smartwatch = c(200,200,200,200,200,150,180)
df = data.frame(day_of_week,number_of_smartwatches_sold,price_per_smartwatch)

#What is the total number of smartwatches sold during the week?
sum(number_of_smartwatches_sold)

#If we want to know the price on Sunday only, we could extract price for Sunday as followed:
df[df$day_of_week == 'Sunday', 'price_per_smartwatch']
df[df$day_of_week == 'Sunday', 3]
df$price_per_smartwatch[df$day_of_week == 'Sunday']

#What is the total sales revenue from smartphones sold in this week? 
sum(number_of_smartwatches_sold * price_per_smartwatch)
#If we want to know how many days have we had smartwatches sold more than 25.
over25 <- number_of_smartwatches_sold > 25
sum(over25 == T)

price_per_smartwatch[c(6,7)]

#return the days when sales were greater than average
df$day_of_week[df$number_of_smartwatches_sold>mean(df$number_of_smartwatches_sold)]
df[df$number_of_smartwatches_sold>mean(df$number_of_smartwatches_sold),"day_of_week"]
df[df$number_of_smartwatches_sold>mean(df$number_of_smartwatches_sold),1]

```
To import dataset to R environment, we first need to set up
#First, set up your working directory, codes for windows users:
setwd('C:/Users/Downloads')
#Read your data to the console
diamondsData = read.csv("diamonds_data.csv") 

mean(diamondsData$carat)
# the average carat size of a Premium cut diamond
mean(diamondsData$carat[diamondsData$cut == 'Premium'])

#**Q3. cut of diamond has the largest variance in carat size
library(dplyr)
d_c <- diamondsData%>%
  group_by(cut) %>%
  summarize(variance = var(carat));d_c
max(d_c$variance, na.rm = T) #na.rm means NA is remove, T by default
d = diamondsData%>%
  group_by(cut) %>%
  summarize(variance = var(carat)) 



#Q4**Compare number of diamonds by cut but only for color "G". 
#Which cut has the least selection (i.e., lowest count) in color G?
dg<- diamondsData %>%
  filter(color == "G") %>%
  group_by(cut)
table(dg$cut) #Compare categories by Frequency or Counts
#get proportions
prop.table(table(dg$cut))
prop.table(dg$cut) #invalid
#Using xtabs
xtabs(~cut,data = dg)

#the average price in Euros of diamonds larger than 2 carat? 
#Assume the conversion is $1 = Euro 0.85.
mean(diamondsData$price[diamondsData$carat > 2]) * 0.85

```
L2. Dat Tidying
```{r}
setwd('C:/Users/IgnacioKleinmanRuiz/Downloads')
birthdays = read.csv('president_birthdays.csv')
heights = read.csv('president_heights.csv')
states = read.csv('president_states.csv')

#The birthdays of US Presidents need to be parsed into the ISO8601 standard for dates. 
library(lubridate)
mdy(birthdays$birthday) 
#or
birthdays$birthday_n <- as.Date(birthdays$birthday,format='%m / %d / %Y')

#Remove that leading space of President name
library(readr)
parse_character(birthdays$Name)
#or
library(stringr)
str_trim(birthdays$Name)

#Which month were the most Presidents born in?
#Use format and as.numeric function to extract birthday
birthdays$month <- as.numeric(format(birthdays$birthday_n,'%m')); birthdays

birthdays %>%
  group_by(month) %>%
  count()) 

#What is the average height?
library(readr)
heights$height_parsed =  parse_number(heights$height)
mean(heights$height_parsed)
#What is the median height in cm? 1 inch = 2.54 cm
heights$height_cm = heights$height_parsed * 2.54
median(heights$height_cm)

#What is President James Madison's standardized height?
heights$height_st = scale(heights$height_parsed, center = TRUE, scale = TRUE)
#heights$height_sd = sd(heights$height_parsed) #this will be standardized deviation
heights %>%
  filter(Name == "James Madison")

#**bin height in inches into a categorical variable. how many Presidents are categorized as being "Short"?
heights <- within(heights, {   
  height_cat4 <- NA # need to initialize variable
  height_cat4[height_parsed <= 66] <- "Short"
  height_cat4[height_parsed > 66 & height_parsed <= 69] <- "Average"
  height_cat4[height_parsed > 69 & height_parsed <= 72] <- "Tall"
  height_cat4[height_parsed > 72] <- "Very Tall"
} )
heights %>%
  filter(height_cat4 == "Short") %>%
  count()


#**How many levels (or categories) does height_cat contain?
library(dplyr); library(forcats)
height_cat = heights %>%
  mutate(height_cat = fct_recode(.f = height_cat4, "Average" = "Short", 
                                 "Average" = "Average","Tall" = "Tall",
                                 "Very Tall" = "Very Tall")) 
class(height_cat$height_cat)
length(height_cat$height_cat)
table(height_cat$height_cat)
#**Join the heights data with states data. 
#*What is the average height (in inches) of Presidents born in New Jersey?
cb <- heights %>% 
  inner_join(states, by = 'Name') #inner join: if one row of the states cannot correspond to the row on heights, it will be deleted
cb$Birth.State_parsed = parse_character(cb$Birth.State)
cb$height_parsed = as.numeric(cb$height_parsed)
mean(cb$height_parsed[cb$Birth.State_parsed == 'New Jersey'])

```

Transform results into a meaningful tall format: model in column 1, metric (which takes on values 'r2', 'cp', and 'rss')  in column 2 and value (which contains numbers representing the three metrics)  in column 3. 
```{r}
model = paste('model',1:10,sep = '')
r2 = c(0.849,0.782,0.853,0.853,0.856,0.855,0.859,0.856,0.859,0.859)
cp = c(3785.13,29492.891,2216.066,2515.617,1122.912,1729.176,11.453,1108.412,5.883,11.752)
rss = c(129345695398,186953511457,125825141230,126496397331,123371039554,124729600876,120875920936,123334065616,120858956753,120872109331)
results = data.frame(model, r2, cp, rss)

library(tidyverse)
results %>% pivot_longer(cols = 2:4, names_to = 'metric', values_to = 'value')
#
```

###Ch2. Data Visualization is a great approach to understand your data and present your results.

library(ggplot2)
