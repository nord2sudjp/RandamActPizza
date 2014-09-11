library(jsonlite)
inTrain <- fromJSON("train.json")
inTest <- fromJSON("test.json")

head(inTrain)
names(inTrain)


#
# cleansing
#

# Remove unnecessary variable

#D delete after dev.
inTrain_w <- head(inTrain, 500)

# all of working varialbe is stored into working01_ then cbind with inTrain_w
w01 <- data.frame(ID=c(1:dim(inTrain_w)[1]))
pizza_class <- data.frame(ID=c(1:dim(inTrain_w)[1]))

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
filteredinTrain
names(filteredinTrain)

#
# prepare pre-testing data
#
pre_intrain <- createDataPartition(y=pizza_class$requester_recieved_pizza_flag, p=0.75, list=FALSE)
pre_training <- filteredinTrain[pre_intrain,]
pre_testing <- filteredinTrain[-pre_intrain,]
dim(pre_training)

pre_training_class <- pizza_class[pre_intrain,]
pre_testing_class <- pizza_class[-pre_intrain,]

#
# lm
#
lm1 <- lm(pre_training_class$requester_recieved_pizza_flag~., data=pre_training)
summary(lm1)
lm1
predict(lm1, data=pre_testing)


#
# glm
#

modelFitGlm <- train(pre_training_class$requester_recieved_pizza_flag~., data=pre_training, method="rf")
modelFitGlm$finalModel

predictGlm <- predict(modelFitGlm, newData=pre_testing)
predictGlm


names(pre_training)
names(pre_testing)
