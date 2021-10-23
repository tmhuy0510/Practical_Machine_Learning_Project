library(lubridate)
library(caret)

training <- read.csv('pml-training.csv')
testing <- read.csv('pml-testing.csv')

# Check what class each column is
# For character class
isCharVec <- rep(NA, times = 160)
for (i in 1:160) {isCharVec[i] <- is.character(training[1, i])}
sum(isCharVec)    # Return 37

# For numeric class
isNumVec <- rep(NA, times = 160)
for (i in 1:160) {isNumVec[i] <- is.numeric(training[1, i])}
sum(isNumVec)     # Return 123
# Sum of 123 and 37 is the number of columns of 'training'

# Further check if columns with character class make sense or not
# The answer is yes
# Further check if columns with character class make sense or not
# The answer is no
names(training)[isCharVec]      

# 'user_name' should be of character class
# 'cvtd_timestamp' should be of date class
training$cvtd_timestamp <- dmy_hm(training$cvtd_timestamp)
# 'new_window' should be of factor class
training$new_window <- as.factor(training$new_window)
# 'classe' should be of factor class
training$classe <- as.factor(training$classe)


# Other columns with character class should be of numeric class
for (i in (1:37)[-c(1, 2, 3, 37)]) {
  training[, isCharVec][, i] <- as.numeric(training[, isCharVec][, i])
  }

# Columns of 'training' are now of appropriate class

# Pay attention to 'new_window' column
table(training$new_window)
# No: 19216 and Yes: 406

numberNAEachCol <- apply(training, 2, function(x) sum(is.na(x)), simplify = T)
colIdxWithNA <- which(numberNAEachCol > 0)
# There are 100 columns with NAs
colIdxWithoutNA <- which(numberNAEachCol == 0)
# There are 60 columns without NAs

# Pay attention to the number of NAs in columns with NAs
table(numberNAEachCol[colIdxWithNA])
# 19216 is the least number of NAs in columns with NAs
# There may be a connection between 'new_window' column and columns with NAs
# Particularly, when 'new_window' = 'no', a certain set of columns may have NAs

# Let's check
subsetNo <- subset(training, new_window == 'no')
# This subset has 19216 rows and 160 columns
apply(subsetNo, 2, function(x) sum(is.na(x)), simplify = T) -> numberNAEachCol2
colIdxWithNA2 <- which(numberNAEachCol2 > 0)
# There are 100 columns with NAs
any(colIdxWithNA2 != colIdxWithNA)
# Result is FALSE so we get the same set of 100 columns with NAs
table(numberNAEachCol2[colIdxWithNA2])
# All of these 100 columns have the same number of NAs which is 19216
# Therefore, when 'new_window' = 'no', a set of 100 columns have NAs


# Create a subset of 'training' with 'new_window' column = 'no' and columns without NAs
trainData <- subset(training, new_window == 'no')[, colIdxWithoutNA]
# Check column names of the subset
names(trainData)
# The first 7 columns can be excluded because they are not relevant to 'classe' column
trainData <- trainData[, -c(1:7)]

set.seed(1234)
inTrain <- createDataPartition(y = trainData$classe, p = 0.7, list = F)
subTrain <- trainData[inTrain, ]
subValidate <- trainData[-inTrain, ]


trainCtrl <- trainControl(method = 'repeatedcv', 
                          number = 5, 
                          repeats = 3)


# Decision Tree

set.seed(1234)
modTree <- train(classe ~ ., 
                 data = subTrain, 
                 method = 'rpart', 
                 trControl = trainCtrl, 
                 tuneLength = 5)
predTree <- predict(modTree, subValidate)
conMatTree <- confusionMatrix(predTree, subValidate$classe)
conMatTree$overall

# Random Forest

set.seed(1234)
modRF <- train(classe ~ .,
               data = subTrain,
               method = 'rf',
               trControl = trainCtrl,
               tuneLength = 5)
predRF <- predict(modRF, subValidate)
conMatRF <- confusionMatrix(predRF, subValidate$classe)
conMatRF$overall

# Gradient Boosted Trees

set.seed(1234)
modGBM <- train(classe ~ .,
               data = subTrain,
               method = 'gbm',
               trControl = trainCtrl,
               tuneLength = 5,
               verbose = F)
predGBM <- predict(modGBM, subValidate)
conMatGBM <- confusionMatrix(predGBM, subValidate$classe)
conMatGBM$overall

# Support Vector Machine

set.seed(1234)
modSVM <- train(classe ~ .,
                data = subTrain,
                method = 'svmLinear',
                preProcess = c('center', 'scale', 'nzv'),
                trControl = trainCtrl,
                tuneLength = 5, 
                verbose = F)
predSVM <- predict(modSVM, subValidate)
conMatSVM <- confusionMatrix(predSVM, subValidate$classe)
conMatSVM$overall

# Linear Discriminant Analysis

set.seed(1234)
modLDA <- train(classe ~ .,
                data = subTrain,
                method = 'lda',
                preProcess = c('center', 'scale', 'nzv'),
                trControl = trainCtrl,
                tuneLength = 5,
                verbose = F)
predLDA <- predict(modLDA, subValidate)
conMatLDA <- confusionMatrix(predLDA, subValidate$classe)
conMatLDA$overall

accuracyDF <- data.frame(Model = c('Tree', 'RF', 'GBM', 'SVM', 'LDA'),
                         Accuracy = c(conMatTree$overall[1],
                                      conMatRF$overall[1],
                                      conMatGBM$overall[1],
                                      conMatSVM$overall[1],
                                      conMatLDA$overall[1]))

# The 'modRF' model will be used to predict the outcomes of 'testing' data

# Transform 'testing' data to new one with appropriate format

# 'user_name' should be of character class
# 'cvtd_timestamp' should be of date class
testing$cvtd_timestamp <- dmy_hm(testing$cvtd_timestamp)
# 'new_window' should be of factor class
testing$new_window <- as.factor(testing$new_window)

# Other columns with character class should be of numeric class
for (i in (1:37)[-c(1, 2, 3, 37)]) {
  testing[, isCharVec][, i] <- as.numeric(testing[, isCharVec][, i])
}

# Pay attention to 'new_window' column
table(testing$new_window)
# All values of 'new_window' column are 'no'
# It will be reasonable to apply 'modRF' model on 'testing' data because the model was built on the assumption of 'new_window' = 0 

# Subset non-NA columns from 'testing' data
testData <- testing[, colIdxWithoutNA]

# Check if there is any NA value in new 'testData' data
sum(sum(is.na(testData)))
# The result is 0

testPred <- predict(modRF, testData)
