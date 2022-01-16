Data Visualization is a great approach to understand your data and present your results.
In R, we use ggplot2 library to visualize our data. The template for graphic in ggplot2 is:
ggplot(data = <Enter Data Here>, mapping = aes(<Enter Aesthetic(s) here>))+ 
        <Enter geom function here>

We will use mpg data for example.
```
library(ggplot2)
str(mpg)
```

To examine distribution, we could use histogram, frequency polygon, desnity curve, boxplot, and QQplot.
Histogram: The most common chart to explore the distribution of a numerical variable.
Let us examine the distribution of highway gas mileage (hwy). 

```
#Histogram
ggplot(data=mpg,mapping = aes(x=hwy))+
  geom_histogram()
#By default, geom_histogram use 30 as number of bins. we could override it by sepcifying bins or binwidth argument.
#To compare distributions, we could put interested variable to fill.
ggplot(data=mpg,mapping = aes(x=hwy,fill=factor(year)))+
  geom_histogram() 
```

Frequnecy Polygon: By replacing geom_histogram() with geom_freqpoly(), we could avoid overlaid bins, and it will be easier to see and compare data. And therefore, in this case, we use color as argument. 
```
ggplot(data=mpg,mapping = aes(x=hwy,color=factor(year)))+
  geom_freqpoly(size=1.2)
```
