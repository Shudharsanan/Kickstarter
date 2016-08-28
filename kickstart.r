#kick data
kick <- read.csv("kick.csv")
str(kick)

table(kick$project_currency)
table(kick$project_category)
table(kick$project_subcategory)

#Feature engineering :(
#kick$project_raw_location <- as.character(kick$project_raw_location)
#location <- strsplit(kick$project_raw_location, split='[,]')[[1]][2]
#locfun <- function(x){strsplit(x, split='[,]')[[1]][2]}
#location <- sapply(kick$project_raw_location , FUN = function(x){strsplit(x, split='[,]')[[1]][2]})
#location <- sub(' ','',location)
#kick$location <- location
kick1 <- na.omit(kick)
#summary(kick)

kick1$success <- ifelse(kick1$percentage_pledged >= 1, 1, 0)
table(kick1$success)

kick_no <- kick1[kick1$success == 0,]
kick_yes <- kick1[kick1$success == 1,]
set.seed(1)
no <- floor(.30*nrow(kick_no))
yes <- floor(.51*nrow(kick_yes))
no_index <- sample(seq_len(nrow(kick_no)),size=no)
yes_index <- sample(seq_len(nrow(kick_yes)),size=yes)

train_no <- kick_no[no_index,]
train_yes <- kick_yes[yes_index,]
test_no <- kick_no[-no_index,]
test_yes <- kick_yes[-yes_index,]
train <- rbind(train_no,train_yes)
test <- rbind(test_no,test_yes)
test_target <- test$success
test$success <- NULL

#Modelling
lm.mod1 <- glm(as.factor(success) ~ project_goal+project_currency+duration+project_category+project_subcategory+project_has_video+project_body_length+project_body_image_count+project_body_video_count+founder_experience+reward_count, data = train,family = binomial)
summary(lm.mod1)

lm.pred1 <- predict(lm.mod1, newdata = test, type = "response")
class <- ifelse(lm.pred1>0.64,1,0)
table(class, test_target)
mean(class!=test_target)
table(test_target)

library(ROCR)
ensem1.prediction <- prediction(lm.pred1, test_target)
ensem1.err <- performance(ensem1.prediction, measure = "err")
as.numeric(performance(ensem1.prediction, "auc")@y.values)
plot(ensem1.err, ylim=c(0.1, 0.5))

################--------------------@@@@@@@@@@@@%%%%%%%%%%%%%%%%%%%%%%%%$$$$$$$$$$$$$$$



#Modelling2
kick_train$reward_backer_limit <- NULL
sum(is.na(kick_train))
kick_train1 <- na.omit(kick_train)
kick_train1$success <- ifelse(kick_train1$project_status == "successful", 1, 0)
kick_yes <- kick_train1[kick_train1$project_status == "successful",]
kick_no <- kick_train1[kick_train1$project_status == "failed",]
set.seed(1)
no <- floor(.30*nrow(kick_no))
yes <- floor(.51*nrow(kick_yes))
no_index <- sample(seq_len(nrow(kick_no)),size=no)
yes_index <- sample(seq_len(nrow(kick_yes)),size=yes)

train_no <- kick_no[no_index,]
train_yes <- kick_yes[yes_index,]
test_no <- kick_no[-no_index,]
test_yes <- kick_yes[-yes_index,]
train <- rbind(train_no,train_yes)
test <- rbind(test_no,test_yes)
test_target <- test$success
test$project_status <- NULL
test$success <- NULL

lm.mod2 <- glm(as.factor(success)~project_goal+project_currency+
               +duration+project_category+project_subcategory+project_has_video+project_body_length+project_body_image_count
               +project_body_video_count+founder_experience+variation_score+max_reward_percent+reward_count, data = kick_train1, family = binomial)


lm.pred2 <- predict(lm.mod2, newdata = test, type = "response")
class <- ifelse(lm.pred2>0.5,1,0)
table(class, test_target)
mean(class!=test_target)

library(ROCR)
ensem2.prediction <- prediction(lm.pred2, test_target)
ensem2.err <- performance(ensem2.prediction, measure = "err")
as.numeric(performance(ensem2.prediction, "auc")@y.values)
plot(ensem2.err, ylim=c(0.1, 0.5))

######################%%%%%%%%%%%%%%%%%%$$$$$$$$$$$$$$$$$$$$$$$&&&&&&&&&&&&&&&&&&&&

#Modelling3
kick_train1 <- read.csv("newtest.csv")
summary(kick_train1)
kick_train1$reward_backer_limit <- NULL

#outlier treatment
#percentage_pledged
plot(kick_train1$project_goal , kick_train1$percentage_pledged)
index<- which(kick_train1$percentage_pledged >10.0)
kick_train1<- kick_train1[-index,]
kick_train1$percentage_pledged[which.max(kick_train1$percentage_pledged)]

#project_goal
plot(kick_train1$project_id , kick_train1$project_goal)
index1<- which(kick_train1$project_goal > 30000000)
kick_train1<- kick_train1[-index1,]
kick_train1$project_goal[which.max(kick_train1$project_goal)]


kick_train1 <- na.omit(kick_train1)
kick_train1$success <- ifelse(kick_train1$project_status == "successful", 1, 0)
kick_yes <- kick_train1[kick_train1$project_status == "successful",]
kick_no <- kick_train1[kick_train1$project_status == "failed",]
set.seed(1)
no <- floor(.30*nrow(kick_no))
yes <- floor(.51*nrow(kick_yes))
no_index <- sample(seq_len(nrow(kick_no)),size=no)
yes_index <- sample(seq_len(nrow(kick_yes)),size=yes)

train_no <- kick_no[no_index,]
train_yes <- kick_yes[yes_index,]
test_no <- kick_no[-no_index,]
test_yes <- kick_yes[-yes_index,]
train <- rbind(train_no,train_yes)
test <- rbind(test_no,test_yes)
test_target <- test$success
test$project_status <- NULL
test$success <- NULL

names(train)
lm.mod3 <- glm(as.factor(success)~project_currency+duration+project_category+project_subcategory+project_has_video+project_body_length+project_body_image_count+project_body_video_count+variation_score+max_reward_percent+reward_count+goal_USD+year, data = train, family = binomial)

lm.pred3 <- predict(lm.mod3, newdata = test, type = "response")
class <- ifelse(lm.pred3>0.7,1,0)
table(class, test_target)
mean(class==test_target)

plot(lm.mod3)

library(ROCR)
ensem3.prediction <- prediction(lm.pred3, test_target)
ensem3.err <- performance(ensem3.prediction, measure = "err")
as.numeric(performance(ensem3.prediction, "auc")@y.values)
plot(ensem3.err, ylim=c(0.1, 0.5))

str(train)

lm.mod2 <- glm(as.factor(success)~project_currency+project_category+project_subcategory+project_has_video+project_body_length+project_body_image_count+project_body_video_count+goal_USD+year, data = train, family = binomial)
vif(lm.mod3)
lm.pred2 <- predict(lm.mod2, newdata = test, type = "response")
class <- ifelse(lm.pred2>0.7,1,0)
table(class, test_target)
mean(class==test_target)

summary(lm.mod3)
library(ROCR)
ensem2.prediction <- prediction(lm.pred2, test_target)
ensem2.err <- performance(ensem2.prediction, measure = "err")
as.numeric(performance(ensem2.prediction, "auc")@y.values)
plot(ensem2.err, ylim=c(0.1, 0.5))
