library(jsonlite)
inTrain <- fromJSON("train.json")
inTest <- fromJSON("test.json")

head(inTrain)
names(inTrain)


##########################################
# cleansing
##########################################

# Remove unnecessary variable

#D delete after dev.
inTrain_w <- head(inTrain, 500)

# all of working varialbe is stored into working01_ then cbind with inTrain_w
w01 <- data.frame(ID=c(1:dim(inTrain_w)[1]))   # create dummy record.
pizza_class <- data.frame(ID=c(1:dim(inTrain_w)[1]))    # pizza_class contain class record.

# giver_user_name_if_known transform into character length.
#D inTrain_w[inTrain_w[1] == "N/A", "", inTrain_w[1]]
#D ifelse(inTrain_w[,1] == "N/A", "", inTrain_w[,1])
w01$giver_username_if_known_len <- nchar(ifelse(inTrain_w[,1] == "N/A", "", inTrain_w[,1]))
# w01$giver_username_if_known_len

#request_text
w01$request_text_len <- nchar(inTrain_w$request_text)
# w01$request_text_len

#request_text_edit_aware
w01$request_text_edit_aware_len <- nchar(inTrain_w$request_text_edit_aware)
# w01$request_text_edit_aware_len

#request_title
w01$request_title_len <- nchar(inTrain_w$request_title)
# w01$request_title_len

#requester_subreddits_at_request
w01$requester_subreddits_at_request_len <- nchar(inTrain_w$requester_subreddits_at_request)
# w01$requester_subreddits_at_request_len

#requester_user_flair
w01$requester_user_flair_len <- nchar(ifelse(is.na(inTrain_w$requester_user_flair), "", inTrain_w$requester_user_flair))
# w01$requester_user_flair_len

#requester_username
w01$requester_username_len <- nchar(inTrain_w$requester_username)
# w01$requester_username_len


pizza_class$requester_recieved_pizza <- inTrain_w$requester_received_pizza
pizza_class$requester_recieved_pizza_flag <- as.factor(inTrain_w$requester_received_pizza)
# pizza_class

# bind working data.frame.
inTrain_w <- cbind(inTrain_w, w01)
head(inTrain_w)
names(inTrain_w)

# now remove unnecessary variable
x <- c(2:4,6,10:22, 25:28, 34:40)
inTrain_w01 <- inTrain_w[,x]

library(caret)


# remove less informational data
nzv <- nearZeroVar(inTrain_w01)
filteredinTrain <- inTrain_w01[,-nzv]
names(filteredinTrain)

# remove variable with strong correlation
trainCor <- cor(filteredinTrain)
highlyCorinTrain <- findCorrelation(trainCor, cutoff=0.75)
filteredinTrain <- filteredinTrain[,-highlyCorinTrain]
names(filteredinTrain)

#
# prepare pre-testing data
#
pre_intrain <- createDataPartition(y=pizza_class$requester_recieved_pizza_flag, p=0.75, list=FALSE)
pre_training <- filteredinTrain[pre_intrain,]
pre_testing <- filteredinTrain[-pre_intrain,]
dim(pre_training)
dim(pre_testing)
#head(pre_training)
#head(pre_testing)

pre_training_class <- pizza_class[pre_intrain,]
pre_testing_class <- pizza_class[-pre_intrain,]

##########################################
# Analysis
##########################################
#
# lm
#
res_lm <- lm(pre_training_class$requester_recieved_pizza~., data=pre_training)
summary(res_lm)
pre_lm<-predict(res_lm, data=pre_testing)
pre_lm

#
# lda  判別分析
#
library(MASS)
res_lda <- lda(pre_training, pre_training_class$requester_recieved_pizza_flag)
table(pre_training_class$requester_recieved_pizza_flag, predict(res_lda)$class)
plot(res_lda, dimen=1)
plot(res_lda, dimen=2)

pre_lda <- predict(res_lda, pre_testing)
table(pre_testing_class$requester_recieved_pizza_flag, pre_lda$class)


#
# decision tree.
#
library(mvpart)
res_tree <- rpart(pre_training_class$requester_recieved_pizza_flag~., data=pre_training, method="class")
plot(res_tree)
text(res_tree, all=TRUE, use.n=TRUE)


#
# random forest
#
res_rf <- train(pre_training_class$requester_recieved_pizza_flag~., data=pre_training, method="rf")
res_rf$finalModel

pre_rf <- predict(res_rf, pre_testing)
pre_rf
table(pre_rf, pre_testing_class$requester_recieved_pizza_flag)

#
# logisitc regression
#
res_glm_binomial <- glm(pre_training_class$requester_recieved_pizza_flag~pre_training[,1], data=pre_training, family=binomial)
x <- summary(res_glm_binomial)
x$coefficients[8]


pre_glm_binomial <- predict(res_glm_binomial, pre_testing)
pre_glm_binomial

dim(pre_testing)



#
# glm 一般化線形モデル
#
res_glm_rf <- glm(pre_training_class$requester_recieved_pizza_flag~., data=pre_training, family=binomial)
summary(res_rf)





pre_rf <- predict(res_glm_binomial, newData=pre_testing)
pre_rf




##########################################
# Debug with iris
##########################################
iris_data <- iris[,c(1:4)]
#iris_data
iris_class <- iris[5] # Factor
#iris_class
iris_train <- createDataPartition(y=iris_class$Species, p=0.75, list=FALSE)
iris_data_train <- iris_data[iris_train,]
iris_data_testing <- iris_data[-iris_train,]

iris_class_train <- data.frame(Species=iris_class[iris_train,])
iris_class_testing <- data.frame(Species=iris_class[-iris_train,])

iris_class_train

# decision tree.
res_tree <- rpart(iris_class_train$Species~., data=iris_data_train, method="class")
plot(res_tree)
text(res_tree, all=TRUE, use.n=TRUE)

# random forest
res_rf <- train(iris_class_train$Species~ ., data=iris_data_train, method="rf", prox=TRUE)
res_rf$finalModel
pre_rf <- predict(res_rf, iris_data_testing)
table(pre_rf, iris_class_testing$Species)


# glm - logistic analysis
res_glm_binomial <- glm(iris_class_train$Species~iris_data_train[,1], data=iris_data_train, family=binomial)
x <- summary(res_glm_binomial)
x$coefficients[8]

func_glm <- function(parm01){
  res_glm_binomial <- glm(iris_class_train$Species~parm01, data=iris_data_train, family=binomial)
  x <- summary(res_glm_binomial)
  x$coefficients[8]
}
func_glm(iris_data_train[,1])

  apply(iris_data_train[1:3], 2, func_glm(x))

iris_data_train[1:3]
dat <- data.frame(x=c(1,2), y=c(3,4), z=c(5,6))
apply(dat[,c('x','z')], 2, function(x) sum(x) )
dat[,c('x','z')]

dat
