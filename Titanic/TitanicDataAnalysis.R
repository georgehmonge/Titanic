library(ggplot2)

train <- read.csv("train.csv", header=TRUE)
test <- read.csv("test.csv", header=TRUE)
test.Survived <- data.frame(Survived = rep("None", nrow(test)), test[,])
test.Survived <- test.Survived[c(2,1,3,4,5,6,7,8,9,10,11,12)]
data.combined <- rbind(train, test.Survived)
str(data.combined)

data.combined$Pclass <- as.factor(data.combined$Pclass)
data.combined$Survived <- as.factor(data.combined$Survived)

table(data.combined$Survived)
table(data.combined$Pclass)

library(ggplot2)
train$Pclass <- as.factor(train$Pclass)
ggplot(train, aes(x = Pclass, fill = factor(Survived))) +
  geom_bar(width = 0.5) +
  xlab("Pclass") +
  ylab("Total Count") +
  labs(fill = "Survived")

head(as.character(train$Name))

length(unique(as.character(data.combined$Name)))

dup.Names <- as.character(data.combined[which(duplicated(as.character(data.combined$Name))), "Name"])

data.combined[which(data.combined$Name %in% dup.Names),]

library(stringr)

misses <- data.combined[which(str_detect(data.combined$Name, "Miss")),]
misses[1:5,]

mrses <- data.combined[which(str_detect(data.combined$Name, "Mrs")),]
mrses[1:5,]

males <- data.combined[which(train$Sex == "male"),]
males[1:5,]

extractTitle <- function(Name) {
  Name <- as.character(Name)
  
  if(length(grep("Miss.", Name)) > 0) {
    return ("Miss.")
  } else if (length(grep("Master.", Name)) > 0) {
    return ("Master.")
  } else if (length(grep("Mrs.", Name)) > 0) {
    return ("Mrs.")
  } else if (length(grep("Mr.", Name)) > 0) {
    return ("Mr.")
  } else {
    return ("Other")
  }
}

titles <- NULL

for (i in 1:nrow(data.combined)) {
  titles <- c(titles, extractTitle(data.combined[i,"Name"]))
}
data.combined$Title <- as.factor(titles) 

ggplot(data.combined[1:891,], aes(x = Title, fill = Survived)) +
  geom_bar(width = 0.5) +
  facet_wrap(~Pclass) +
  ggtitle("Pclass") +
  xlab("Title") +
  ylab("Total Count") +
  labs(fill = "Survived")

table(data.combined$Sex)

ggplot(data.combined[1:891,], aes(x = Sex, fill = Survived)) +
  geom_bar(width = 0.5) +
  facet_wrap(~Pclass) +
  ggtitle("Pclass") +
  xlab("Sex") +
  ylab("Total Count") +
  labs(fill = "Survived")

summary(data.combined$Age)
summary(data.combined[1:891,"Age"])

ggplot(data.combined[1:891,], aes(x = Age, fill = Survived)) +
  facet_wrap(~Sex + Pclass) +
  geom_histogram(binwidth = 10) +
  xlab("Age") +
  ylab("Total Count")

boys <- data.combined[which(data.combined$Title == "Master."),]
summary(boys$Age)

misses <- data.combined[which(data.combined$Title == "Miss."),]
summary(misses$Age)

ggplot(misses[misses$Survived != "None",], aes(x = Age, fill = Survived)) +
  facet_wrap(~Pclass) +
  geom_histogram(binwidth = 5) +
  ggtitle("Age of 'Miss.' by Pclass") +
  xlab("Age") +
  ylab("Total Count")

misses.alone <- misses[which(misses$SibSp == 0 & misses$Parch == 0),]
summary(misses.alone$Age)
length(which(misses.alone$Age <= 14.5))

summary(data.combined$SibSp)

length(unique(data.combined$SibSp))

data.combined$SibSp <- as.factor(data.combined$SibSp)

ggplot(data.combined[1:891,], aes(x = SibSp, fill = Survived)) +
  geom_bar(width = 1) +
  facet_wrap(~Pclass + Title) +
  ggtitle("Pclass, Title") +
  xlab("SibSp") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")

data.combined$Parch <- as.factor(data.combined$Parch)
ggplot(data.combined[1:891,], aes(x = Parch, fill = Survived)) +
  geom_bar(width = 1) +
  facet_wrap(~Pclass + Title) +
  ggtitle("Pclass, Title") +
  xlab("ParCh") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")

temp.Sibsp <- c(train$SibSp, test$SibSp)
temp.Parch <- c(train$Parch, test$Parch)
data.combined$Family.size <- as.factor(temp.Sibsp + temp.Parch + 1)

ggplot(data.combined[1:891,], aes(x = Family.size, fill = Survived)) +
  geom_bar(width = 1) +
  facet_wrap(~Pclass + Title) +
  ggtitle("Pclass, Title") +
  xlab("Family.size") +
  ylab("Total Count") +
  labs(fill = "Survived")

str(data.combined$Ticket) 

data.combined$Ticket <- as.character(data.combined$Ticket)
data.combined$Ticket[1:20]

Ticket.first.char <- ifelse(data.combined$Ticket == "", " ", substr(data.combined$Ticket, 1, 1))
unique(Ticket.first.char)

data.combined$Ticket.first.char <- as.factor(Ticket.first.char)

ggplot(data.combined[1:891,], aes(x = Ticket.first.char, fill = Survived)) +
  geom_bar() +
  ggtitle("Survivability by Ticket.first.char") +
  xlab("Ticket.first.char") +
  ylab("Total Count") +
  ylim(0,350) +
  labs(fill = "Survived")

ggplot(data.combined[1:891,], aes(x = Ticket.first.char, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass) +
  ggtitle("Pclass") +
  xlab("Ticket.first.char") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")

ggplot(data.combined[1:891,], aes(x = Ticket.first.char, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + Title) +
  ggtitle("Pclass, Title") +
  xlab("Ticket.first.char") +
  ylab("Total Count") +
  ylim(0,200) +
  labs(fill = "Survived")

summary(data.combined$Fare)
length(unique(data.combined$Fare))

ggplot(data.combined, aes(x = Fare)) +
  geom_histogram(binwidth = 5)+
  ggtitle("Combined Fare Distribution") +
  xlab("Fare") +
  ylab("Total Count") +
  ylim(0,200)

ggplot(data.combined[1:891,], aes(x = Fare, fill = Survived)) +
  geom_histogram(binwidth = 5) +
  facet_wrap(~Pclass + Title) +
  ggtitle("Pclass, Title") +
  xlab("Fare") +
  ylab("Total Count") +
  ylim(0,50) +
  labs(fill = "Survived")

str(data.combined$Cabin)

data.combined$Cabin <- as.character(data.combined$Cabin)
data.combined$Cabin[1:100]

data.combined[which(data.combined$Cabin == ""), "Cabin"] <- "U"
data.combined$Cabin[1:100]

Cabin.first.char <- as.factor(substr(data.combined$Cabin, 1, 1))
str(Cabin.first.char)
levels(Cabin.first.char)

data.combined$Cabin.first.char <- Cabin.first.char

ggplot(data.combined[1:891,], aes(x = Cabin.first.char, fill = Survived)) +
  geom_bar() +
  ggtitle("Survivability by Cabin.first.char") +
  xlab("Cabin.first.char") +
  ylab("Total Count") +
  ylim(0,750) +
  labs(fill = "Survived")

ggplot(data.combined[1:891,], aes(x = Cabin.first.char, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass) +
  ggtitle("Survivability by Cabin.first.char") +
  xlab("Pclass") +
  ylab("Total Count") +
  ylim(0,500) +
  labs(fill = "Survived")

ggplot(data.combined[1:891,], aes(x = Cabin.first.char, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + Title) +
  ggtitle("Pclass, Title") +
  xlab("Cabin.first.char") +
  ylab("Total Count") +
  ylim(0,500) +
  labs(fill = "Survived")

data.combined$Cabin.multiple <- as.factor(ifelse(str_detect(data.combined$Cabin, " "), "Y", "N"))

ggplot(data.combined[1:891,], aes(x = Cabin.multiple, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + Title) +
  ggtitle("Pclass, Title") +
  xlab("Cabin.multiple") +
  ylab("Total Count") +
  ylim(0,350) +
  labs(fill = "Survived")

str(data.combined$Embarked)
data.combined$Embarked <- as.factor(data.combined$Embarked)
levels(data.combined$Embarked)

ggplot(data.combined[1:891,], aes(x = Embarked, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + Title) +
  ggtitle("Pclass, Title") +
  xlab("Embarked") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")

#Exploratory Modeling

library(randomForest)

#Train a Random Forest with the default parameters using Pclass and Title
rf.train.1 <- data.combined[1:891, c("Pclass", "Title")]
rf.label <- as.factor(train$Survived)

set.seed(1234)
rf.1 <- randomForest(x = rf.train.1, y = rf.label, importance = TRUE, ntree = 1000)
rf.1
varImpPlot(rf.1)

rf.train.2 <- data.combined[1:891, c("Pclass", "Title", "SibSp")]

set.seed(1234)
rf.2 <- randomForest(x = rf.train.2, y = rf.label, importance = TRUE, ntree = 1000)
rf.2
varImpPlot(rf.2)

rf.train.3 <- data.combined[1:891, c("Pclass", "Title", "Parch")]

set.seed(1234)
rf.3 <- randomForest(x = rf.train.3, y = rf.label, importance = TRUE, ntree = 1000)
rf.3
varImpPlot(rf.3)

rf.train.4 <- data.combined[1:891, c("Pclass", "Title", "SibSp", "Parch")]

set.seed(1234)
rf.4 <- randomForest(x = rf.train.4, y = rf.label, importance = TRUE, ntree = 1000)
rf.4
varImpPlot(rf.4)

rf.train.5 <- data.combined[1:891, c("Pclass", "Title", "Family.size")]

set.seed(1234)
rf.5 <- randomForest(x = rf.train.5, y = rf.label, importance = TRUE, ntree = 1000)
rf.5
varImpPlot(rf.5)

rf.train.6 <- data.combined[1:891, c("Pclass", "Title", "SibSp", "Family.size")]

set.seed(1234)
rf.6 <- randomForest(x = rf.train.6, y = rf.label, importance = TRUE, ntree = 1000)
rf.6
varImpPlot(rf.6)

rf.train.7 <- data.combined[1:891, c("Pclass", "Title", "Parch", "Family.size")]

set.seed(1234)
rf.7 <- randomForest(x = rf.train.7, y = rf.label, importance = TRUE, ntree = 1000)
rf.7
varImpPlot(rf.7)

#Cross Validation

test.submit.df <- data.combined[892:1309, c("Pclass", "Title", "Family.size")]

rf.5.preds <- predict(rf.5, test.submit.df)
table(rf.5.preds)

submit.df <- data.frame(PassengerId = rep(892:1309), Survived = rf.5.preds)


write.csv(submit.df, file = "RF_SUB_20200618_1.csv", row.names = FALSE)

# Our submission scores 0.79426, but the OOB predicts that we should score 0.8159
# Let's look into cross-validation using the caret package to see if we can get
# more accurate estimates

library(caret)
library(doSNOW)

#Research has shown that 10-fold CV (Cross Validation) repeated 10 times is the best place to start,
#However there are no hard and fast rules - this is where the experience of the
#Data Scientist (i.e., the "art") come into play. We'll start with 10-fold CV,
#repeated 10 times and see how it goes

#Leverage caret to create 100 total folds, but ensure that the ratio of those
#that survived and perished in each fold matches the overall training set. This
#is known as stratified cross validation and generally provided better results.


set.seed(2348)
cv.10.folds <- createMultiFolds(rf.label, k = 10, times = 10)

table(rf.label)
342 / 549

table(rf.label[cv.10.folds[[33]]])
308 / 494

ctrl.1 <- trainControl(method = "repeatedcv", number = 10, repeats = 10,
                       index = cv.10.folds)

# Set up doSNOW package for multi-core training. This is helpful as we're going
# to be training a lot of trees.
# NOTE - This works on Windows and Mac, unlike doMC
cl <- makeCluster(6, type = "SOCK")
registerDoSNOW(cl)

#Set seed for reproducibility and train
library(e1071)
set.seed(34324)
rf.5.cv.1 <- train(x = rf.train.5, y = rf.label, method = "rf", tunelength = 3,
                   ntree = 1000, trControl = ctrl.1)

#Shutdown cluster
stopCluster(cl)

#Check out results
rf.5.cv.1

#Repeat the cross validation with small number
#of folds. Less data to do algorithm on.
#The above is only slightly more pessimistic than the rf.5 OOB prediction, but
#not pessimistic enough. Let's try 5-fold CV repeated 10 times.

set.seed(5983)
cv.5.folds <- createMultiFolds(rf.label, k = 5, times = 10)

ctrl.2 <- trainControl(method = "repeatedcv", number = 5, repeats = 10,
                       index = cv.5.folds)

cl <- makeCluster(6, type = "SOCK")
registerDoSNOW(cl)

set.seed(89472)
rf.5.cv.2 <- train(x = rf.train.5, y = rf.label, method = "rf", tuneLength = 3,
                   ntree = 1000, trControl = ctrl.2)


#Shutdown Cluster
stopCluster(cl)

#CHeck out results
rf.5.cv.2


set.seed(37596)
cv.3.folds <- createMultiFolds(rf.label, k = 3, times = 10)

ctrl.3 <- trainControl(method = "repeatedcv", number = 3, repeats = 10,
                       index = cv.3.folds)

cl <- makeCluster(6, type = "SOCK")
registerDoSNOW(cl)

set.seed(94622)
rf.5.cv.3 <- train(x = rf.train.5, y = rf.label, method = "rf", tuneLength = 3,
                   ntree = 64, trControl = ctrl.3)


#Shutdown Cluster
stopCluster(cl)

#CHeck out results
rf.5.cv.3


#Exploratory Modeling 2

#Let's use a single decision tree to better understand what's going on with our
#features. Obviously Random Forests are far more powerful than single trees,
#but single trees have the advantage of being easier to understand.

library(rpart)
library(rpart.plot)

#Per video #5, let's use 3-fold DC repeated 10 times

#Create utility function
rpart.cv <- function(seed, training, labels, ctrl) {
  cl <- makeCluster(6, type = "SOCK")
  registerDoSNOW(cl)
  
  set.seed(seed)
  #Leverage formula interface for training
  rpart.cv <- train(x = training, y = labels, method = "rpart", tuneLength = 30,
                    trControl = ctrl)
  
  #Shutdown Cluster
  stopCluster(cl)
  
  return (rpart.cv)
}

#Grab features
features <- c("Pclass", "Title", "Family.size")
rpart.train.1 <- data.combined[1:891, features]

#Run CV and check out results
rpart.1.cv.1 <- rpart.cv(94622, rpart.train.1, rf.label, ctrl.3)
rpart.1.cv.1

#Plot
prp(rpart.1.cv.1$finalModel, type = 0, extra = 1, under = TRUE)

#The plot bring out some interesting lines of investigation. Namely:
#     1 - Titles of "Mr." and "Other" are predicted to perish at an
#         overall accuracy rate of 83.2 %
#     2 - Titles of "Master.", "Miss.", and & "Mrs." in 1st and 2nd class
#         are predicted to survive at an overall accuracy of 94.9%
#     3 - Titles of "Master.", "Miss.", and & "Mrs." in 3rd class with
#         family sizes equal to 5, 6, 8, & 11 are predicted to perish
#         with 100% accuracy
#     4 - Titles of "Master.", "Miss.", and & "Mrs." in 3rd class with
#         family sizes not equal to 5, 6, 8, & 11 are predicted to
#         survive with 59.6% accuracy.


#Both rpart and rf confirm that title is important, let's investigate further
table(data.combined$Title)

#Parse out last name and title
data.combined[1:25, "Name"]

Name.splits <- str_split(data.combined$Name, ",")
Name.splits[1]
last.Names <- sapply(Name.splits, "[", 1)
last.Names[1:10]

#Add last names to dataframe in case we find it useful later
data.combined$last.Names <- last.Names

#Now for titles
Name.splits <- str_split(sapply(Name.splits, "[", 2), " ")
Name.splits[1]
Titles <- sapply(Name.splits, "[", 2)
unique(Titles)

#What's up with a title of 'the'?
data.combined[which(Titles == "the"),]

#Re-map titles to be more exact
# same as which(Titles == "Dona." | Titles == "the")
Titles[Titles %in% c("Dona.", "the")] <- "Lady."
Titles[Titles %in% c("Ms.", "Mlle.")] <- "Miss."
Titles[Titles == "Mme."] <- "Mrs."
Titles[Titles %in% c("Jonkheer.", "Don.")] <- "Sir."
Titles[Titles %in% c("Col.", "Capt.", "Major.")] <- "Officer"
table(Titles)

#Make title a factor

data.combined$new.Title <- as.factor(Titles)

#Visualize new version of title
ggplot(data.combined[1:891,], aes(x = new.Title, fill = Survived)) +
  geom_bar() +
  facet_wrap(~ Pclass) +
  ggtitle("Survival Rates for new.Title by Pclass")

#Collapse titles based on visual analysis
indexes <- which(data.combined$new.Title == "Lady.")
data.combined$new.Title[indexes] <- "Mrs."

indexes <- which(data.combined$new.Title == "Dr." |
                 data.combined$new.Title == "Rev." |
                 data.combined$new.Title == "Sir." |
                 data.combined$new.Title == "Officer")

data.combined$new.Title[indexes] <- "Mr."

ggplot(data.combined[1:891,], aes(x = new.Title, fill = Survived)) +
  geom_bar() +
  facet_wrap(~ Pclass) +
  ggtitle("Survival Rates for new.Title by Pclass")

#Grab features
features <- c("Pclass", "new.Title", "Family.size")
rpart.train.2 <- data.combined[1:891, features]

#Run CV and check out results
rpart.2.cv.1 <- rpart.cv(94622, rpart.train.2, rf.label, ctrl.3)
rpart.2.cv.1

#Plot
prp(rpart.2.cv.1$finalModel, type = 0, extra = 1, under = TRUE)

#Dive in on 1st class "Mr."
data.combined$Sex <- as.factor(data.combined$Sex)
indexes.first.mr <- which(data.combined$new.Title == "Mr." & data.combined$Pclass == "1")
first.mr.df <- data.combined[indexes.first.mr, ]
summary(first.mr.df)

#One female?
first.mr.df[first.mr.df$Sex == "female",]

#Update new.Title feature
indexes <- which(data.combined$new.Title == "Mr." &
                 data.combined$Sex == "female")
data.combined$new.Title[indexes] <- "Mrs."

#Any other gender slip-ups?
length(which(data.combined$Sex == "female" &
               (data.combined$new.Title == "Master." |
                data.combined$new.Title == "Mr.")))

#End 6/30 at 54:00
#Refresh Data Frame
indexes.first.mr <- which(data.combined$new.Title == "Mr." & data.combined$Pclass == "1")
first.mr.df <- data.combined[indexes.first.mr,]

#Let's look at surviving 1st class "Mr."
summary(first.mr.df[first.mr.df$Survived == "1",])
View(first.mr.df[first.mr.df$Survived == "1",])

#Take a look at some of the high fares
indexes <- which(data.combined$Ticket == "PC 17755" |
                 data.combined$Ticket == "PC 17611" |
                 data.combined$Ticket == "113760")
View(data.combined[indexes,])

#Visualize survival rates for 1st class "Mr." by fare
ggplot(first.mr.df, aes(x = Fare, fill = Survived)) +
  geom_density(alpha = 0.5) +
  ggtitle("1st Class 'Mr.' Survival Rates by fare")

# Engineer features based on all passengers with the same ticket
Ticket.Party.Size <- rep(0, nrow(data.combined))
Avg.fare <- rep(0.0, nrow(data.combined))
Tickets <- unique(data.combined$Ticket)

for (i in 1:length(Tickets)) {
  current.Ticket <- Tickets[i]
  party.indexes <- which(data.combined$Ticket == current.Ticket)
  current.avg.fare <- data.combined[party.indexes[1], "Fare"] / length(party.indexes)
  
  for(k in 1:length(party.indexes)) {
    Ticket.Party.Size[party.indexes[k]] <- length(party.indexes)
    Avg.fare[party.indexes[k]] <- current.avg.fare
    
  } 
}

data.combined$Ticket.Party.Size <- Ticket.Party.Size
data.combined$Avg.fare <- Avg.fare

#Refresh 1st class "Mr." dataframe
first.mr.df <- data.combined[indexes.first.mr, ]
summary(first.mr.df)

#Visualize new features
ggplot(first.mr.df[first.mr.df$Survived != "None",], aes(x = Ticket.Party.Size, fill = Survived)) +
  geom_density(alpha = 0.5) +
  ggtitle("Survival Rates 1st Class 'Mr.' by Ticket.Party.Size")

ggplot(first.mr.df[first.mr.df$Survived != "None",], aes(x = Avg.fare, fill = Survived)) +
  geom_density(alpha = 0.5) +
  ggtitle("Survival Rates 1st Class 'Mr.' by Avg.fare")

#Hypothesis - Ticket.Party.Size is highly correlated with Avg.fare
summary(data.combined$Avg.fare)

#One missing value, take a look
data.combined[is.na(data.combined$Avg.fare),]

#Get records for similar passengers and summarize Avg.fares
indexes <- with(data.combined, which(Pclass == "3" & Title == "Mr." & Family.size == 1 &
                                       Ticket != "3701"))
similar.na.passengers <- data.combined[indexes,]
summary(similar.na.passengers$Avg.fare)

#Use median since close to mean and a little higher than mean
data.combined[is.na(Avg.fare), "Avg.fare"] <- 7.840

#Leverage caret's preProcess function to normalize data
preproc.data.combined <- data.combined[, c("Ticket.Party.Size", "Avg.fare")]
preProc <- preProcess(preproc.data.combined, method = c("center", "scale"))

postproc.data.combined <- predict(preProc, preproc.data.combined)

#Hypothesis refuted for all data
cor(postproc.data.combined$Ticket.Party.Size, postproc.data.combined$Avg.fare)

# How about for just 1st class all-up?
indexes <- which(data.combined$Pclass == "1")
cor(postproc.data.combined$Ticket.Party.Size[indexes],
    postproc.data.combined$Avg.fare[indexes])
#Hypothesis refuted again

# OK, let's see if our feature engineering has made any difference
features <- c("Pclass", "new.Title", "Family.size", "Ticket.Party.Size", "Avg.fare")
rpart.train.3 <- data.combined[1:891, features]

#Run CV and check out results
rpart.3.cv.1 <- rpart.cv(94622, rpart.train.3, rf.label, ctrl.3)
rpart.3.cv.1

#Plot
prp(rpart.3.cv.1$finalModel, type = 0, extra = 1, under = TRUE)

#Submitting

test.submit.df <- data.combined[892:1309, features]

#Make Prediction

rpart.3.preds <- predict(rpart.3.cv.1$finalModel, test.submit.df, type = "class")
table(rpart.3.preds)

#Write out a CSV file for submission to Kaggle
submit.df <- data.frame(PassengerId = rep(892:1309), Survived = rpart.3.preds)

write.csv(submit.df, file = "RPART_SUB_20200707_1.csv", row.names = FALSE)

#Random forest scores 

features <- c("Pclass", "new.Title", "Ticket.Party.Size", "Avg.fare")
rf.train.temp <- data.combined[1:891, features]

set.seed(1234)
rf.temp <- randomForest(x = rf.train.temp, y = rf.label, ntree = 1000)
rf.temp

test.submit.df <- data.combined[892:1309, features]

#Make Predictions
rf.preds <- predict(rf.temp, test.submit.df)
table(rf.preds)

#Write out a CSV file for submission to Kaggle
submit.df <- data.frame(PassengerId = rep(892:1309), Survived = rf.preds)

write.csv(submit.df, file = "RF_SUB_20200707_1.csv", row.names = FALSE)


#If we want to improve our model, a good place to start is focusing on where it
#gets thing wrong

#First, let's explore our collection of features using mutual information to
#gain some additional insight. Out intuition is that the plot of our tree
#should align well to the definition of mutual information.
#install.packages("infotheo")

#07/07 at 33:50

library(infotheo)

mutinformation(rf.label, data.combined$Pclass[1:891])
mutinformation(rf.label, data.combined$Sex[1:891])
mutinformation(rf.label, data.combined$SibSp[1:891])
mutinformation(rf.label, data.combined$Parch[1:891])
mutinformation(rf.label, discretize(data.combined$Fare[1:891]))
mutinformation(rf.label, data.combined$Embarked[1:891])
mutinformation(rf.label, data.combined$Title[1:891])
mutinformation(rf.label, data.combined$Family.size[1:891])
mutinformation(rf.label, data.combined$Ticket.first.char[1:891])
mutinformation(rf.label, data.combined$Cabin.multiple[1:891])
mutinformation(rf.label, data.combined$new.Title[1:891])
mutinformation(rf.label, data.combined$Ticket.Party.Size[1:891])
mutinformation(rf.label, discretize(data.combined$Avg.fare[1:891]))

#OK, now let's leverage the tsne algorithm to create a 2-D presentation of our data
#suitable for visualization starting with folks our model gets right very often - females
#and boys.
#install.packages("Rtsne")
library(Rtsne)
most.correct <- data.combined[data.combined$new.Title != "Mr.",]
indexes <- which(most.correct$Survived != "None")

tsne.1 <- Rtsne(most.correct[, features], check_duplicates = FALSE)
ggplot(NULL, aes(x = tsne.1$Y[indexes ,1], y = tsne.1$Y[indexes, 2],
                 color = most.correct$Survived[indexes])) +
  geom_point() +
  labs(color = "Survived") +
  ggtitle("tsne 2D Visualization of Features for Females and Boys")

#To get a baseline, let's use conditional mutual information on the tsne X and
#Y features for females and boys in 1st and 2nd class. The intuition here is that
#the combination of these features should be higher than any individual features
#we looked at above.
condinformation(most.correct$Survived[indexes], discretize(tsne.1$Y[indexes,]))

#As one more comparison, we can leverage conditional mutual information using
#the top two features used in our tree plot - new.Title and Pclass
condinformation(rf.label, data.combined[1:891, c("new.Title", "Pclass")])

#OK, now let's take a look at adult males since our model has the biggest
#potential upside for improving(i.e., the tree predicts incorrectly for 86
# adult males). Let's visualize with tsne.
misters <- data.combined[data.combined$new.Title == "Mr.",]
indexes <- which(misters$Survived != "None")

tsne.2 <- Rtsne(misters[, features], check_duplicates = FALSE)
ggplot(NULL, aes(x = tsne.2$Y[indexes ,1], y = tsne.2$Y[indexes, 2],
                 color = misters$Survived[indexes])) +
  geom_point() +
  labs(color = "Survived") +
  ggtitle("tsne 2D Visualization of Features for new.Title of 'Mr.'")

#Now conditional mutual information for tsne features for adult males
condinformation(misters$Survived[indexes], discretize(tsne.2$Y[indexes,]))


#
#Idea - How about creating tsne features for all of the training data and
#using them in our model?
#
tsne.3 <- Rtsne(data.combined[, features], check_duplicates = FALSE)
ggplot(NULL, aes(x = tsne.3$Y[1:891, 1], y = tsne.3$Y[1:891, 2],
                 color = misters$Survived[1:891])) +
  geom_point() +
  labs(color = "Survived") +
  ggtitle("tsne 2D Visualization of Features for all Training Data")


condinformation(data.combined$Survived[1:891], discretize(tsne.3$Y[1:891,]))

#Add the tsne features to our data frame for use is model building

data.combined$tsne.x <- tsne.3$Y[,1]
data.combined$tsne.y <- tsne.3$Y[,2]






