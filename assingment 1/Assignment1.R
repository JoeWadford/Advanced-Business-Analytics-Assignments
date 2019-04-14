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

