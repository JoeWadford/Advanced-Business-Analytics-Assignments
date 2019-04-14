Assignment 1: Advanced Classification 
Student Name: W. Joseph Wadford
1. Use train_data to build a random forest classifier with 10 trees. Use library(randomForest). 
1.1. (2 points) What is the OOB estimate of error rate?  9.02%
1.2. (2 points) How many variables R tried at each split?   5
1.3. (4 points) Now use 20 trees. 
1.3.1.  What is OOB estimate of error rate?  8.46%
1.3.2.  How many variables R tried at each split?  5
1.4. (4 points) Now use 30 trees. 
1.4.1.  What is OOB estimate of error rate?  5.85%
1.4.2.  How many variables R tried at each split? 5
1.5. (2 points) Increase the number of trees in 10 increments (e.g. 40, 50, …). Using OOB error rate to evaluate your random forest classifier, how many trees would you recommend? 
1.5.1. I would recommend 60 trees
1.6. (2 points) Use tuneRF() function to find the best value for mtry. Here is the code:
mtry <- tuneRF(train_data[-26], train_data$gen_election, ntreeTry=n,  stepFactor=1.5, improve=0.01, trace=TRUE, plot=TRUE, , na.action=na.exclude). Replace n with the number of trees you recommended in 9.5. What is the recommended value for mtry?  4
1.7. (2 points)  Use your recommended number of trees and mtry value to build a new random forest classifier using train_data. What is OOB estimate of error rate?  6.77%
1.8. (8 points) Use library(caret)1 and the code in Module 6 to create the confusion matrix for test_data. Fill out the confusion matrix in below. Use “W” as the value of option positive in confusionMatrix() function. 
ActualPredictedWLW12411L9135
1.8.1. What is the value of accuracy?  0.9283
1.8.2. What is the value of TPR?  0.9323
1.8.3. What is the value of FPR?  0.0753
1.9. (4 points) Use the code in Module 6 to calculate AUC and create the ROC curve. 
1.9.1.  What is the value of AUC?   0.983185703985992
1.9.2. Paste the ROC curve in the space below:

1.10. (4 points) Use varImpPlot() to create the plot for variable importance. What are the type five important variables when we use MeanDecreaseAccuracy?
1.10.1. (Opp_fund) Opposition fund, ending cash, contributions from other political committees, beginning cash, and facebook.

2. Use library(nnet) and the code in Module 6 to build a neural network classifier. 
2.1. (20 points) Use 5 hidden nodes in your ANN. 
2.1.1. How many input nodes are in the ANN?   39
2.1.2. How many weights are in the ANN?   206
2.1.3. Use library(caret) and the code in Module” 6 to create the confusion matrix for test_data. Fill out the confusion matrix in below. Use “W” as the value of option positive in confusionMatrix() function.
ActualPredictedWLW10013L33133
2.1.4. What is the value of sensitivity? 0.7519
2.1.5. What is the value of specificity? 0.9110
2.1.6. Use the code in Module 6 to calculate AUC and create the ROC curve. 
2.1.6.1. What is the value of AUC? 0.910958904109589
2.1.6.2. Paste the ROC curve in the space below:

2.2. (6 points) Increase the number of hidden nodes until you get the following error: “Error in nnet.default(x, y, w, entropy = TRUE, ...): too many (1026) weights.” Use the maximum number of hidden nodes that you can use to build your ANN classifier. 
2.2.1. What is the maximum number of hidden nodes that we could use?  24
2.2.2. Use the code in Module 6 to calculate AUC and create the ROC curve. 
2.2.2.1. What is the value of AUC?  0.946827685652487
2.2.2.2. Paste the ROC curve in the space below:


3. (5 points) Among the three classifiers that you built, which classifier would you finally use for predicting the election’s outcome? Please explain.
3.1. I would use the random forest classifier for predicting the election’s outcome.  The AUC for the ROC curve is the highest for the random forest classifier.  This means that the random forest classifier maximizes our ability to find a decision that maximizes our specificity and sensitivity.  In simpler, yet vaguer, terms – a AUC closer to 1 increases our ability to discriminate between correctly and incorrectly predicting a general election win or loss.  

4. (10 points) The buzz from the 2008 election motivated the candidates for political offices to employ social media campaigns to get their message across. Imagine that you are an advisor to a candidate who is running for a Congressional seat. Based on your analysis, would you recommend sparing money and resources to create social media campaigns? If so, among the three social media platforms (Facebook, Twitter, and YouTube), which platform would you recommend to invest in? Please explain. You can use function ftable() in Module 3 to support your recommendation. 
4.1. Generally speaking, campaigns with social media accounts had much better win-loss ratios than campaigns without social media accounts.   After creating a subset of the test data where coh_bop > $50,000 and comparing that to another subset wherein coh_cop < $50,000, the results become much more clear.   When candidates had more than $50,000 at the beginning of their campaign, the difference in win-loss ratio was significant but not as dramatic as when candidates had less than $50,000.  In other words, when candidates had less money – having a social media account made a huge difference.  Based on this table alone, I would conclude that having a social media account is very important ESPECIALLY if your candidate has less money. 

It seems as though having a facebook is the most important social media account to have but facebook also seems to work well in conjunction with twitter and youtube.  
4.2. 

5. (10 points) Given your analysis, would you agree with this statement: “Money Buys Political Power”? Please explain. 
5.1. Yes, money does buy political power.  You can see that the more money a campaign has, the more likely it is to win (see table below in section 5.2).   However if you review the analysis for question 4, you can see that social media helps to reduce the effect of money in politics.  
5.2. 

6. (10 points) Imagine that you are an advisor to a candidate who is running for a Congressional seat. Based on your analysis, what are your prescriptions for success for your candidate? Please explain.
6.1. My advice would be to network in order to maximize donations, especially from other political committees.  Additionally, I would highly recommend that my candidate hire social media experts to leverage the power of social media in order to maximize the efficiency of their cash flow.  I would also highly recommend utilizing the social media capabilities of other political committees to her/his benefit.  For reference see the answers for questions: 1.10.1; 1.9.2; 4.1; 4.2; 5.1; 5.2.

7. (5 points) Please paste your R code in the space below:
data <- read.csv("~/MBAD 6211 advanced business analytics/assignments/election_campaign_data.csv", sep=",", header=T, strip.white=T, na.strings=c("NA","NaN","","?"))

summary(data)

data$cand_id <- NULL
data$last_name <- NULL
data$first_name <- NULL
data$twitterbirth <- NULL
data$facebookdate <- NULL
data$facebookjan <- NULL
data$youtubebirth <- NULL
data$twitter <- factor(data$twitter)
data$facebook <- factor(data$facebook)
data$youtube <- factor(data$youtube)
data$cand_ici <- factor(data$cand_ici)
data$gen_election <- ifelse(data$gen_election == "W", 1, 0)
data$gen_election <- factor(data$gen_election)

summary(data)

set.seed(32)

data<- data[complete.cases(data), ]

complete.cases(data)

n = nrow(data) 

trainIndex = sample(1:n, 
                    size = round(0.7*n), 
                    replace=FALSE) 

train_data = data[trainIndex,] 
test_data = data[-trainIndex,]

summary(train_data)

library(randomForest)

rf <-randomForest(gen_election~., data=train_data, ntree=10, na.action=na.exclude, importance=T,
                  proximity=T) 
print(rf)

rf <-randomForest(gen_election~., data=train_data, ntree=20, na.action=na.exclude, importance=T,
                  proximity=T) 
print(rf)

rf <-randomForest(gen_election~., data=train_data, ntree=30, na.action=na.exclude, importance=T,
                  proximity=T) 
print(rf)

rf <-randomForest(gen_election~., data=train_data, ntree=40, na.action=na.exclude, importance=T,
                  proximity=T) 
print(rf)

rf <-randomForest(gen_election~., data=train_data, ntree=50, na.action=na.exclude, importance=T,
                  proximity=T) 
print(rf)

rf <-randomForest(gen_election~., data=train_data, ntree=60, na.action=na.exclude, importance=T,
                  proximity=T) 
print(rf)

rf <-randomForest(gen_election~., data=train_data, ntree=70, na.action=na.exclude, importance=T,
                  proximity=T) 
print(rf)

rf <-randomForest(gen_election~., data=train_data, ntree=80, na.action=na.exclude, importance=T,
                  proximity=T) 
print(rf)

rf <-randomForest(gen_election~., data=train_data, ntree=90, na.action=na.exclude, importance=T,
                  proximity=T) 
print(rf)

rf <-randomForest(gen_election~., data=train_data, ntree=100, na.action=na.exclude, importance=T,
                  proximity=T) 
print(rf)

rf <-randomForest(gen_election~., data=train_data, ntree=110, na.action=na.exclude, importance=T,
                  proximity=T) 
print(rf)

rf <-randomForest(gen_election~., data=train_data, ntree=120, na.action=na.exclude, importance=T,
                  proximity=T) 
print(rf)

rf <-randomForest(gen_election~., data=train_data, ntree=130, na.action=na.exclude, importance=T,
                  proximity=T) 
print(rf)

rf <-randomForest(gen_election~., data=train_data, ntree=140, na.action=na.exclude, importance=T,
                  proximity=T) 
print(rf)

rf <-randomForest(gen_election~., data=train_data, ntree=150, na.action=na.exclude, importance=T,
                  proximity=T) 
print(rf)

rf <-randomForest(gen_election~., data=train_data, ntree=160, na.action=na.exclude, importance=T,
                  proximity=T) 
print(rf)

rf <-randomForest(gen_election~., data=train_data, ntree=170, na.action=na.exclude, importance=T,
                  proximity=T) 
print(rf)

rf <-randomForest(gen_election~., data=train_data, ntree=180, na.action=na.exclude, importance=T,
                  proximity=T) 
print(rf)

rf <-randomForest(gen_election~., data=train_data, ntree=190, na.action=na.exclude, importance=T,
                  proximity=T) 
print(rf)

rf <-randomForest(gen_election~., data=train_data, ntree=200, na.action=na.exclude, importance=T,
                  proximity=T) 
print(rf)

rf <-randomForest(gen_election~., data=train_data, ntree=210, na.action=na.exclude, importance=T,
                  proximity=T) 
print(rf)

rf <-randomForest(gen_election~., data=train_data, ntree=220, na.action=na.exclude, importance=T,
                  proximity=T) 
print(rf)

rf <-randomForest(gen_election~., data=train_data, ntree=230, na.action=na.exclude, importance=T,
                  proximity=T) 
print(rf)

rf <-randomForest(gen_election~., data=train_data, ntree=240, na.action=na.exclude, importance=T,
                  proximity=T) 
print(rf)


set.seed(32)
mtry <- tuneRF(train_data[-26], train_data$gen_election, ntreeTry=60,  stepFactor=1.5, improve=0.01, trace=TRUE, plot=TRUE, , na.action=na.exclude)

best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]

print(mtry)

print(best.m)

set.seed(32)
rf <-randomForest(gen_election~., data=train_data, mtry=best.m, importance=TRUE, ntree=60)
print(rf)

library(caret)
library("e1071")

set.seed(32)
predicted_values <- predict(rf, test_data,type= "prob")
head(predicted_values)

threshold <- 0.5
pred <- factor( ifelse(predicted_values[,2] > threshold, 1, 0) )
levels(test_data$gen_election)[2]

confusionMatrix(pred, test_data$gen_election, 
                positive = levels(test_data$gen_election)[2])

set.seed(32)
predicted_values <- predict(rf, test_data, type = "prob")[,2]
pred <- prediction(predicted_values, test_data$gen_election)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]

roc.data <- data.frame(fpr=unlist(perf@x.values),
                       tpr=unlist(perf@y.values),
                       model="RF")
ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) +
  geom_ribbon(alpha=0.2) +
  geom_line(aes(y=tpr)) +
  ggtitle(paste0("ROC Curve w/ AUC=", auc))

set.seed(32)
varImpPlot(rf)

set.seed(32)
ann <- nnet(gen_election ~ ., data=train_data, size=5, maxit=1000)

summary(ann)

set.seed(32)
predicted_values_ann <- predict(ann, test_data, type = "raw")
head(predicted_values)
threshold <- 0.5
pred_ann <- factor(ifelse(predicted_values_ann[,1] > threshold, 1, 0))
head(pred)

levels(test_data$gen_election)[2]

confusionMatrix(pred_ann, test_data$gen_election,
                  positive = levels(test_data$gen_election)[2])

pred_ann <- prediction(predicted_values_ann, test_data$gen_election)
perf_ann <- performance(pred_ann, measure = "tpr", x.measure = "fpr")
auc <-performance(pred_ann, measure="auc")
auc <- auc@y.values[[1]]
roc.data_ann <- data.frame(fpr=unlist(perf_ann@x.values),
                           tpr=unlist(perf_ann@y.values),
                                      model="ANN")
ggplot(roc.data_ann, aes(x=fpr, ymin=0, ymax=tpr)) +
  geom_ribbon(alpha=0.2) +
  geom_line(aes(y=tpr)) +
  ggtitle(paste0("ROC Curve w/ AUC=", auc))

set.seed(32)
ann <- nnet(gen_election ~ ., data=train_data, size=24, maxit=1000)

predicted_values_ann <- predict(ann, test_data, type = "raw")
head(predicted_values)

pred_ann <- prediction(predicted_values_ann, test_data$gen_election)
perf_ann <- performance(pred_ann, measure = "tpr", x.measure = "fpr")
auc <-performance(pred_ann, measure="auc")
auc <- auc@y.values[[1]]
roc.data_ann <- data.frame(fpr=unlist(perf_ann@x.values),
                           tpr=unlist(perf_ann@y.values),
                           model="ANN")
ggplot(roc.data_ann, aes(x=fpr, ymin=0, ymax=tpr)) +
  geom_ribbon(alpha=0.2) +
  geom_line(aes(y=tpr)) +
  ggtitle(paste0("ROC Curve w/ AUC=", auc))

set.seed(32)
ftable(xtabs(~facebook+twitter+youtube+gen_election, data=test_data))
campaignfunds <- test_data[which(test_data$coh_bop>50000),]
ftable(xtabs(~facebook+twitter+youtube+gen_election, data=campaignfunds))
lesscampaignfunds <- test_data[which(test_data$coh_bop<50000),]
ftable(xtabs(~facebook+twitter+youtube+gen_election, data=lesscampaignfunds))

set.seed(32)
lesscampaignfunds <- test_data[which(test_data$coh_bop<50000),]
table(lesscampaignfunds$gen_election)

campaignfunds <- test_data[which(test_data$coh_bop>50000),]
table(campaignfunds$gen_election)

morecampaignfunds <- test_data[which(test_data$coh_bop>250000),]
table(morecampaignfunds$gen_election)

mostcampaignfunds <- test_data[which(test_data$coh_bop>500000),]
table(mostcampaignfunds$gen_election)

1 You may get an error that will require you to use library(e1071). In this case, install this library using install.packages(“e1071”).
---------------

------------------------------------------------------------

---------------

------------------------------------------------------------

11


