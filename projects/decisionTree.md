L7. Decision Tree

```{r}
setwd('C:/Users/IgnacioKleinmanRuiz/Downloads')
#Which of the following variables are non-metric?
wages = read.csv("assignment7_wages.csv",stringsAsFactors = T)
head(wages)

#Some of the values of earn in this dataset are below 0. 
#Remove these data points by running the following code
wages = wages[wages$earn>0,]
#**What fraction (a number between 0 and 1) of the respondents are female?
wage_f = summary(wages$sex == "female")
859/(509+859)
#Or
female <- wages[wages$sex == "female",]
nrow(female)/nrow(wages)

#**Which of following races on average earns the least?
library(dplyr)
wages %>%
  group_by(race) %>%
  summarize(avg_earn = mean(earn)) %>%
  arrange(avg_earn) %>%
  ungroup()

#simple random sampling by employing the sample() function. Use a seed of 1731. 75%
set.seed(1731)
split = sample(x = 1:nrow(wages),size = 0.75*nrow(wages))
split[1:10]
train = wages[split,]
test = wages[-split,]
mean(train$earn)
mean(train$height)

#Sec2
model1 = lm(earn~., data=train)
summary(model1)

#What is the RMSE for model1 on the train sample? (Since tree models don’t generate an R2, let us compute the root mean squared error (RMSE). We will compare linear regression to a regression model later)
pred = predict(model1)
rmse1 = sqrt(mean((pred-train$earn)^2)); rmse1
#Or
library(Metrics)
rmse(pred, train$earn)

#Visualize this interaction effect by running the code below to plot a bar chart.  
library(ggplot2)
ggplot(data=train,aes(y=earn,x=ed))+ 
  geom_bar(stat="summary",fun="mean",fill='cadetblue')+
  facet_wrap(~sex)+
  theme_bw()

#Q4. plots a regression between ed and earn separately for men and women.
ggplot(data=train,aes(y=earn,x=ed,color=sex))+  
  geom_smooth(method="lm",se=F,size=1.2)+  
  scale_x_continuous(breaks=c(seq(2,20,2)))+  
  scale_y_continuous(breaks=c(seq(0,100000,10000)))+
  theme_bw()

model_sex_ed = lm(earn~sex + ed + sex*ed,data=train) 
summary(model_sex_ed)

#Q7. What is the RMSE for model2, incorporating all the variables in model1 and the interaction between sex and ed. 
model2 = lm(earn~. + sex*ed, data=train)
pred2 = predict(model2)
rmse2 = sqrt(mean((pred2-train$earn)^2)); rmse2

model3 = lm(earn~. + sex*ed +sex*age, data=train)
pred3 = predict(model3)
rmse3 = sqrt(mean((pred3-train$earn)^2)); rmse3

model4 = lm(earn~. + sex*ed +sex*age + age*ed, data=train)
pred4 = predict(model4)
rmse4 = sqrt(mean((pred4-train$earn)^2)); rmse4

#construct a model called model5 that considers all possible pairwise interactions. Adding all possible interactions can be tedious, so we will use a shortcut.
model5 = lm(earn~(height+sex+race+ed+age)^2,data=train)
pred5 = predict(model5)
rmse5 = sqrt(mean((pred5-train$earn)^2)); rmse5
summary(model5)
summary(model1)


#Sec3
library(rpart); library(rpart.plot)
# Since we are estimating a regression tree(rather than a classification tree), do not add method=“class” argument, we will set the method to anova.
#Which is the first variable to be used for the split?
tree1 = rpart(earn~.,data = train, method = 'anova')
prp(tree1,digits=5)   # tree plot method 1
rpart.plot(tree1, digits=5) # tree plot method 2
#there's 11 leaves in tree1

#RMSE for tree1?
pred_t1 = predict(tree1)
rmse(train$earn, pred_t1) 
#or
rmse_t1 = sqrt(mean((pred_t1-train$earn)^2)); rmse_t1

#Q6. Change the defaults for the tree model by, first reducing complexity and then adding complexity. We use the minbucket parameter, one of many ways to change complexity of the default tree.
treeSimp1 = rpart(earn~.,data=train,control=rpart.control(minbucket=20))
rpart.plot(treeSimp1, digits=5)
pred_ts1 = predict(treeSimp1)
rmse(train$earn, pred_ts1) #the RMSE for treeSimp1

#even simpler tree with minbucket of 50. RMsE becomes bigger
treeSimp2 = rpart(earn~.,data=train,control=rpart.control(minbucket=50))
rpart.plot(treeSimp2, digits=5)
pred_ts2 = predict(treeSimp2)
rmse(train$earn, pred_ts2)

treeComplex1 = rpart(earn~.,data=train,control=rpart.control(minbucket=5))
rpart.plot(treeComplex1, digits=5)
pred_tc1 = predict(treeComplex1)
rmse(train$earn, pred_tc1)

treeComplex2 = rpart(earn~.,data=train,control=rpart.control(minbucket=1))
rpart.plot(treeComplex2, digits=5)
pred_tc2 = predict(treeComplex2)
rmse(train$earn, pred_tc2)
#treeComplex2 has the lowest RMSE among all tree models

#Sec4
#What is the test RMSE for the linear regression model with the lowest train RMSE?
pred5 = predict(model5, newdata=test)
rmse(test$earn, pred5)
#OR
rmse5 = sqrt(mean((pred5-test$earn)^2)); rmse5

#Q2What is the test set RMSE for the tree1?
predt = predict(tree1, newdata=test)
rmse(test$earn, predt)

predt2 = predict(treeSimp2, newdata=test)
rmse(test$earn, predt2)

predt3 = predict(treeComplex2, newdata=test)
rmse(test$earn, predt3)
#among all 4 models, tree1 performed the best on the test sample!
```

L8. Advanced Decisions Trees

```{r}
library(ISLR)
data(OJ)
#Q1. Use the caTools package to do the split and set seed to 1234.70-30%
#How many rows are in the train sample?
library(caTools)
set.seed(1234)
split = sample.split(Y = OJ$Purchase, SplitRatio = 0.7)
split[1:10]
train = OJ[split,]
test = OJ[!split,]
nrow(train)

#In the train sample, how many Minute Maid purchases were made?
nrow(train[train$Purchase == "MM",])

#Q3. What is the average Price for Minute Maid (in the train sample)?
mean(train$PriceMM)

mean(train$DiscMM)

#**Q5.How many purchases of Minute Maid were made in Week 275?
nrow(train[train$WeekofPurchase == 275 & train$Purchase == "MM",])


#Sec2
#Q1. Classification Tree to predict Purchase. What is the Area Under the Curve (AUC) for the test sample? 
tree1 = rpart(Purchase~PriceCH+PriceMM+DiscCH+DiscMM+SpecialCH+SpecialMM+LoyalCH+
                PriceDiff+PctDiscMM+PctDiscCH,data = train, method = 'class')
pred_tcc1 = predict(tree1, newdata = test)
library(ROCR)
ROCRPred = prediction(predictions = pred_tcc1[,2], labels = test$Purchase)
auc = as.numeric(performance(prediction.obj = ROCRPred,measure = 'auc')@y.values); auc

#Q2. Tune the model to optimize complexity. Use 10-fold cross-validation and test cp values ranging from 0 to 0.1 in steps of 0.001. 
#What is the optimal cp?
#Be sure to set seed to 100 just before running the train function.
#Tuned Random Forest can be tuned to improve model predictions
library(caret)
trControl = trainControl(method='cv',number = 10)
tuneGrid = expand.grid(.cp = seq(from = 0.001,to = 0.1,by = 0.001))
set.seed(100)
cvModel = train(Purchase~PriceCH+PriceMM+DiscCH+DiscMM+SpecialCH+
                  SpecialMM+LoyalCH+PriceDiff+PctDiscMM+PctDiscCH,
                data=train,
                method="rpart",
                trControl = trControl,
                tuneGrid = tuneGrid)

cvModel$results
library(ggplot2)
ggplot(data=cvModel$results, aes(x=cp, y=Accuracy))+
  geom_line(size=0.5,alpha=0.2)+
  geom_point(color='brown')+
  theme_bw()+
  ggtitle(label=paste('Lowest RMSE is at a cp of ',cvModel$bestTune$cp))

#Q3. Rerun the tree model with the optimal cp value. What is the auc for this model on the test sample?
tree2 = rpart(Purchase~PriceCH+PriceMM+DiscCH+DiscMM+SpecialCH+
                SpecialMM+LoyalCH+PriceDiff+PctDiscMM+PctDiscCH, data = train, 
              method = 'class', control = rpart.control(cp = cvModel$bestTune$cp))
rpart.plot(tree2)

pred2_test = predict(tree2, newdata = test)[,2]
library(ROCR)
ROCRPred2 = prediction(predictions = pred2_test, labels = test$Purchase)
auc2 = as.numeric(performance(prediction.obj = ROCRPred,measure = 'auc')@y.values); auc2


#Sec3
#bag model. Set seed to 617. Use 1000 trees. In order to get an AUC, 
#we need to get the prediction probability for each prediction. For this purpose, 
#in the predict function, use argument type = "prob" and use the second column of the output. 
#Bc we are running a bag model here, one has to set a value for mtry.
#Q1. What is the auc for the test sample? Round your answer to two decimal places
library(randomForest)
set.seed(617)
bag = randomForest(Purchase~PriceCH+PriceMM+DiscCH+DiscMM+SpecialCH+
                     SpecialMM+LoyalCH+PriceDiff+PctDiscMM+PctDiscCH,
                   data=train,mtry = ncol(train)-1,ntree=1000)
pred = predict(bag,type = "prob", newdata=test)[,2]
#rmse_bag = sqrt(mean((pred-test$earn)^2)); rmse_bag
ROCRPred = prediction(predictions = pred, labels = test$Purchase)
auc = as.numeric(performance(prediction.obj = ROCRPred, measure = 'auc')@y.values); round(auc,2)

#Next, let us construct a random forest model. Use 1000 trees. 
#Do not set mtry as we will use the default. As in the case of the bag model, for the predict function, use argument type = "prob" and use the second column of the output.
#Q2. What is the auc for the test sample? 

library(randomForest)
set.seed(617)
forest = randomForest(Purchase~PriceCH+PriceMM+DiscCH+DiscMM+SpecialCH+
                        SpecialMM+LoyalCH+PriceDiff+PctDiscMM+PctDiscCH,
                      data=train, ntree = 1000)
pred3_test = predict(forest, type = "prob", newdata = test)[,2]
#rmse_forest = sqrt(mean((pred-test$Purchase)^2)); rmse_forest. will return NA
ROCRPred3 = prediction(predictions = pred3_test, labels = test$Purchase)
auc3 = as.numeric(performance(prediction.obj = ROCRPred3,measure = 'auc')@y.values); round(auc3,2)


#The levels of variable Purchase are 1 and 2. But, in order 
#to run a boosting model for a two-level classification model, the dependent variable can only take  values 0 and 1. 
#For the new variable, 0 represents CH and 1 represents MM
train$Purchase2 = as.numeric(train$Purchase)-1
test$Purchase2 = as.numeric(test$Purchase)-1

#Use this new variable Purchase2 and not Purchase. Run a gradient boosting model (gbm) with 1000 trees using Purchase2. 
#Set distribution to "bernoulli", interaction depth to 1 and shrinkage parameter to 0.04. 
#Set seed to 617 just before the step where you run the gradient boosting model. 
#In the predict function, use argument type = "response" and set n.trees = 100. 
#Unlike the randomForest package you do not need to request the second column. 
#Probabilities generated are the chance of purchasing the juice labeled as 1 which is Minute Maid.
library(gbm)
set.seed(617)
boost = gbm(Purchase2~PriceCH+PriceMM+DiscCH+DiscMM+SpecialCH+SpecialMM+LoyalCH+PriceDiff+PctDiscMM+PctDiscCH,
            data=train,
            distribution="bernoulli",
            n.trees = 100,
            interaction.depth = 1,
            shrinkage = 0.04)
#Q3. What is the auc for this model on the test sample?
#rmse_boost_train = sqrt(mean((pred-train$earn)^2)); rmse_boost_train
pred_4 = predict(boost,newdata=test,n.trees = 100)
#rmse_boost = sqrt(mean((pred_4-test$earn)^2)); rmse_boost
ROCRPred4 = prediction(predictions = pred_4, labels = test$Purchase2)
auc4 = as.numeric(performance(prediction.obj = ROCRPred4,measure = 'auc')@y.values); round(auc4,2)

````