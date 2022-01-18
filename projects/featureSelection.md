L6. Feature Selection
A simple method for screening predictors is by examining their bivariate correlations.
* Relevant: High bivariate correlation with outcome 
* Non-redundant: Low bivariate correlation with other predictors
A correlation matrix tabulates all pairwise correlations among variables in a dataset.
```{r}
setwd('C:/Users/IgnacioKleinmanRuiz/Downloads')
houses = read.csv("houses (1).csv")
library(caret)
set.seed(1031)
split = createDataPartition(y=houses$price,p = 0.7,list = F,groups = 100)
train = houses[split,]
test = houses[-split,]
str(train)
mean(train$price) #not asking houses here

#Q2. Examine bivariate correlations with price to identify variables that are weakly related to (or not relevant) for predicting price. Which of the following variables has the weakest relationship with price?
str(train)
library(tidyr); library(dplyr); library(ggplot2)
corMatrix = as.data.frame(cor(train[,-16]))
corMatrix$var1 = rownames(corMatrix)

corMatrix %>%
  gather(key=var2,value=r,1:11)%>%
  arrange(var1,desc(var2))%>%
  ggplot(aes(x=var1,y=reorder(var2, order(var2,decreasing=F)),fill=r))+
  geom_tile()+
  geom_text(aes(label=round(r,2)),size=3)+
  scale_fill_gradientn(colours = c('#d7191c','#fdae61','#ffffbf','#a6d96a','#1a9641'))+
  theme(axis.text.x=element_text(angle=75,hjust = 1))+xlab('')+ylab('')

#Or
round(cor(train[,-16]), 2)*100

#examine correlations among the predictors. Visualize the correlations by running this:
library(ggcorrplot);library(car);library(leaps)
ggcorrplot(cor(train[,c(3:7, 10:13,16)]),type = 'lower',show.diag = F,colors = c('red','white','darkgreen'))

#To verify this by computing the correlation between sqft_living and the sum of sqft_above and sqft_basement. 
cor(train$sqft_living, train$sqft_above + train$sqft_basement)
#or
model = lm(sqft_living~sqft_above+sqft_basement,train)
summary(model)
library(broom)
summary(model) %>%
  tidy()

#The threat of collinearity can also come from linear relationships between sets of variables. One way to assess the threat of multicollinearity in a linear regression is to compute the Variance Inflating Factor (VIF).
#Run a multiple regression model
model1 = lm(price~bedrooms+bathrooms+sqft_living+sqft_lot+floors+ waterfront+view+condition+grade+age, data = train)

library(car)
vif(model1)
#VIF>10 indicates seious multicollinearity while 5<VIF<10 may warrant examination.

#Sec2
##$Subset Selection###
#install.packages('leaps')
library(leaps)
subsets = regsubsets(price~bedrooms+bathrooms+sqft_living+ sqft_lot+ floors+waterfront+
                       view+condition+grade+age, data=train, nvmax=10)
summary(subsets)
#based on the results, the top 6 predictors are bedroom, sqft_living, waterfront, view, age, grade

#What is the R2 for the best six-predictor model?
model_sub = lm(price~bedrooms+sqft_living+ waterfront+view+grade+age, data=train)
summary(model_sub)

#Run a forward stepwise regression model
#To implement this method we provide step() with a null model, a full model, and a model to start with (in this case, also null) and specify direction as forward. 
start_mod = lm(price~1,data=train)
empty_mod = lm(price~1,data=train)
full_mod = lm(price~bedrooms+bathrooms+sqft_living+ sqft_lot+ floors+waterfront+view+condition+grade+age,data=train)
forwardStepwise = step(start_mod,
                       scope=list(upper=full_mod,lower=empty_mod),
                       direction='forward')
#Results show variables being added successively until marginal improvement in AIC is no longer significant.

#Q4backward stepwise regression model (not sure about this!)
start_mod = lm(price~bedrooms+bathrooms+sqft_living+ sqft_lot+ floors+waterfront+view+condition+grade+age,data=train)

empty_mod = lm(price~1,data=train)

full_mod = lm(price~bedrooms+bathrooms+sqft_living+ sqft_lot+ floors+waterfront+view+condition+grade+age,data=train)

backwardStepwise = step(start_mod,
                        scope=list(upper=full_mod,lower=empty_mod),direction='backward')

#a hybrid stepwise regression model
start_mod = lm(price~1,data=train)
empty_mod = lm(price~1,data=train)
full_mod = lm(price~bedrooms+bathrooms+sqft_living+ sqft_lot+ floors+waterfront+view+condition+grade+age,data=train)
hybridStepwise = step(start_mod,
                      scope=list(upper=full_mod,lower=empty_mod),
                      direction='both')



```