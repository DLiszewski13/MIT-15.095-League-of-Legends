######### League of Legends

# load packages
library(caret)
library(rpart)
#install.packages('rpart.plot')
library(rpart.plot)
library(caTools)
library(dplyr)
library(randomForest)
library(ROCR)
library(ggcorrplot)

# set directory
setwd('/Users/pan/Dropbox (MIT)/ML_league/data/Imputed_Final')
getwd()

# read file
train <- read.csv('train_1row_2019_imputed_full.csv')

corrplot <- subset(train,select=-result)
corrplot_ind <- unlist(lapply(corrplot, is.numeric)) 
corrplot = corrplot[ , corrplot_ind]
ggcorrplot(cor(corrplot),type='lower',ggtheme = ggplot2::theme_gray,colors = c("#6D9EC1", "white", "#E46726"))

nrow(train)/4

train_blue = train[1:5098,]
train_red = train[15295:20392,]
train = full_join(train_blue,train_red)

train <- train %>% select(-league)
train$result = as.factor(train$result)
levels(train$result) <- c("loss", "win") 
train$playoffs = as.factor(train$playoffs)
str(train)

test = read.csv('test_1row_2019_imputed_full.csv') 
test$result = as.factor(test$result)
levels(test$result) <- c("loss", "win")
test$playoffs = as.factor(test$playoffs)
test <- test %>% select(-league)
nrow(test)

test <- test %>% filter(champion_top %in% train$champion_top) %>% filter(champion_jng %in% train$champion_jng) %>% 
  filter(champion_bot %in% train$champion_bot)%>% filter(champion_mid %in% train$champion_mid)%>% filter(champion_sup %in% train$champion_sup) %>% 
  filter(champion_top_enemy %in% train$champion_top_enemy) %>% filter(champion_jng_enemy %in% train$champion_jng_enemy) %>% 
  filter(champion_bot_enemy %in% train$champion_bot_enemy)%>% filter(champion_mid_enemy %in% train$champion_mid_enemy)%>% filter(champion_sup_enemy %in% train$champion_sup_enemy)

nrow(test)

########################### CART cross-validation

tuning_vals <- data.frame(.cp = seq(0, 0.015, by=0.001))

cpCV = train(result~.,
                trControl=trainControl(method="cv",number=5, summaryFunction=twoClassSummary, classProbs=TRUE), data=train, method="rpart",minbucket=5,
                tuneGrid=tuning_vals, metric="ROC", maximize=TRUE)

ggplot(data=cpCV$results) +
  geom_line(aes(x=cp, y=ROC,col='1'), lwd=2) +
  theme_bw() +
  xlab("cp") +
  ylab("ROC") +
  theme(axis.title=element_text(size=14), axis.text=element_text(size=14), legend.position='none')

cpCV$bestTune

dc = rpart(result ~., method = 'class',data=train, cp=0.01, minbucket = 5)
prp(dc,digits = 3, varlen = 0, faclen = 0, tweak = 1)

preds <- predict(dc, newdata = test, type="class")
conf.mtx = table(test$result, preds)
accuracy = (conf.mtx[1,1] + conf.mtx[2,2])/nrow(test)
accuracy

preds <- predict(dc, newdata = test, type="prob")
rocr.pred <- prediction(preds[,2], test$result)
auc <- as.numeric(performance(rocr.pred, "auc")@y.values)
auc

#------------------------------------------------------------------------------------------------------------------------------------------------

rf = randomForest(result ~ ., data=train, ntree=400, mtry=2)

ntree = 200
rf <- train(x = train %>% select(-c(result)),
               y = train$result,
               method="rf",
               metric="Accuracy",
               tuneGrid=data.frame(mtry=seq(2, 10, by=2)),
               trControl=trainControl(method="oob"),
               ntree=ntree)

mtry_best = rf$bestTune[[1]]
mtry_best

preds <- predict(rf, newdata = test, type="raw")
conf.mtx = table(test$result, preds)
accuracy = (conf.mtx[1,1] + conf.mtx[2,2])/nrow(test)
accuracy

preds <- predict(rf, newdata = test, type="prob")
rocr.pred <- prediction(preds[,2], test$result)
auc <- as.numeric(performance(rocr.pred, "auc")@y.values)
auc

importance.rf <- data.frame(imp=importance(rf))
importance.rf <- as.data.frame(importance.rf)
importance.rf <- data.frame(Importance_level = importance.rf$MeanDecreaseGini,
                            Words = rownames(importance.rf))
imp_sorted = importance.rf[order(importance.rf$Importance_level,decreasing = T),]
top20_imp = imp_sorted[1:30,]

top20_imp$Words <- factor(top20_imp$Words, levels = top20_imp$Words)
ggplot(top20_imp) +
  geom_bar(aes(x=Words,y = Importance_level),stat="identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

##### ------------------------------ 2019 0.5/0.5 ------------------------------------------------------------------------------
# read file
train <- read.csv('/Users/pan/Dropbox (MIT)/ML_league/data/Imputed_Final/train_0.5_2019_imputed_full.csv')

corrplot <- subset(train,select=-result)
corrplot_ind <- unlist(lapply(corrplot, is.numeric)) 
corrplot = corrplot[ , corrplot_ind]
ggcorrplot(cor(corrplot),type='lower',ggtheme = ggplot2::theme_gray,colors = c("#6D9EC1", "white", "#E46726"))

train$result = as.factor(train$result)
levels(train$result) <- c("loss", "win") 
train$playoffs = as.factor(train$playoffs)

selected_features = c('result','player_wr_year_sup_enemy','avg_mk_enemy_year_mid_enemy','avg_wardskilled_year_mid','n_games_champions_year_sup_enemy','player_wr_6mon_jng_enemy','avg_kda_6mon_sup','avg_gddiff_10_year_top_enemy',
           'avg_gddiff_10_6mon_top_enemy','avg_dpm_year_top_enemy','avg_dpm_6mon_top_enemy','avg_gddiff_15_year_sup_enemy')

train_selected = train %>% select(selected_features)
ncol(train_selected)

test = read.csv('/Users/pan/Dropbox (MIT)/ML_league/data/Imputed_Final/test_0.5_2019_imputed_full.csv') 
test$result = as.factor(test$result)
levels(test$result) <- c("loss", "win")
test$playoffs = as.factor(test$playoffs)

test_selected = test %>% select(selected_features)
ncol(test_selected)
########################### CART cross-validation

tuning_vals <- data.frame(.cp = seq(0, 0.015, by=0.001))

cpCV = train(result~.,
             trControl=trainControl(method="cv",number=5, summaryFunction=twoClassSummary, classProbs=TRUE), data=train, method="rpart",minbucket=5,
             tuneGrid=tuning_vals, metric="ROC", maximize=TRUE)

ggplot(data=cpCV$results) +
  geom_line(aes(x=cp, y=ROC,col='1'), lwd=2) +
  theme_bw() +
  xlab("cp") +
  ylab("ROC") +
  theme(axis.title=element_text(size=14), axis.text=element_text(size=14), legend.position='none')

cpCV$bestTune

dc = rpart(result ~., method = 'class',data=train_selected, cp=0.08, minbucket = 5)
prp(dc,digits = 3, varlen = 0, faclen = 0, tweak = 1)

preds <- predict(dc, newdata = test_selected, type="class")
conf.mtx = table(test_selected$result, preds)
accuracy = (conf.mtx[1,1] + conf.mtx[2,2])/nrow(test)
accuracy

preds <- predict(dc, newdata = test_selected, type="prob")
rocr.pred <- prediction(preds[,2], test_selected$result)
auc <- as.numeric(performance(rocr.pred, "auc")@y.values)
auc

#------------------------------------------------------------------------------------------------------------------------------------------------

rf = randomForest(result ~ ., data=train_selected, ntree=500, mtry=3)

ntree = 200
rf <- train(x = train %>% select(-c(result)),
            y = train$result,
            method="rf",
            metric="Accuracy",
            tuneGrid=data.frame(mtry=seq(2, 10, by=2)),
            trControl=trainControl(method="oob"),
            ntree=ntree)

mtry_best = rf$bestTune[[1]]
mtry_best

preds <- predict(rf, newdata = test_selected, type="response")
conf.mtx = table(test_selected$result, preds)
accuracy = (conf.mtx[1,1] + conf.mtx[2,2])/nrow(test)
accuracy

preds <- predict(rf, newdata = test_selected, type="prob")
rocr.pred <- prediction(preds[,2], test_selected$result)
auc <- as.numeric(performance(rocr.pred, "auc")@y.values)
auc

importance.rf <- data.frame(imp=importance(rf))
importance.rf <- as.data.frame(importance.rf)
importance.rf <- data.frame(Importance_level = importance.rf$MeanDecreaseGini,
                            Words = rownames(importance.rf))
imp_sorted = importance.rf[order(importance.rf$Importance_level,decreasing = T),]
top20_imp = imp_sorted[1:20,]

top20_imp$Words <- factor(top20_imp$Words, levels = top20_imp$Words)
ggplot(top20_imp) +
  geom_bar(aes(x=Words,y = Importance_level),stat="identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

########################### CART cross-validation (whole dataset)

tuning_vals <- data.frame(.cp = seq(0, 0.015, by=0.001))

cpCV = train(result~.,
             trControl=trainControl(method="cv",number=5, summaryFunction=twoClassSummary, classProbs=TRUE), data=train, method="rpart",minbucket=5,
             tuneGrid=tuning_vals, metric="ROC", maximize=TRUE)

ggplot(data=cpCV$results) +
  geom_line(aes(x=cp, y=ROC,col='1'), lwd=2) +
  theme_bw() +
  xlab("cp") +
  ylab("ROC") +
  theme(axis.title=element_text(size=14), axis.text=element_text(size=14), legend.position='none')

cpCV$bestTune

dc = rpart(result ~., method = 'class',data=train, cp=0.001, minbucket = 5)
prp(dc,digits = 3, varlen = 0, faclen = 0, tweak = 1)

preds <- predict(dc, newdata = test, type="class")
conf.mtx = table(test$result, preds)
accuracy = (conf.mtx[1,1] + conf.mtx[2,2])/nrow(test)
accuracy

preds <- predict(dc, newdata = test, type="prob")
rocr.pred <- prediction(preds[,2], test_selected$result)
auc <- as.numeric(performance(rocr.pred, "auc")@y.values)
auc

#------------------------------------------------------------------------------------------------------------------------------------------------

rf = randomForest(result ~ ., data=train, ntree=500, mtry=3)

ntree = 200
rf <- train(x = train %>% select(-c(result)),
            y = train$result,
            method="rf",
            metric="Accuracy",
            tuneGrid=data.frame(mtry=seq(2, 10, by=2)),
            trControl=trainControl(method="oob"),
            ntree=ntree)

mtry_best = rf$bestTune[[1]]
mtry_best

preds <- predict(rf, newdata = test, type="response")
conf.mtx = table(test_selected$result, preds)
accuracy = (conf.mtx[1,1] + conf.mtx[2,2])/nrow(test)
accuracy

preds <- predict(rf, newdata = test, type="prob")
rocr.pred <- prediction(preds[,2], test_selected$result)
auc <- as.numeric(performance(rocr.pred, "auc")@y.values)
auc

importance.rf <- data.frame(imp=importance(rf))
importance.rf <- as.data.frame(importance.rf)
importance.rf <- data.frame(Importance_level = importance.rf$MeanDecreaseGini,
                            Words = rownames(importance.rf))
imp_sorted = importance.rf[order(importance.rf$Importance_level,decreasing = T),]
top20_imp = imp_sorted[1:20,]

top20_imp$Words <- factor(top20_imp$Words, levels = top20_imp$Words)
ggplot(top20_imp) +
  geom_bar(aes(x=Words,y = Importance_level),stat="identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
