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

```{r}

```
