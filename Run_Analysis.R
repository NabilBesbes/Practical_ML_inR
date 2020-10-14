"""
This is Week 4 assignement of 'Pratical Machine Learning' in R 
https://www.coursera.org/learn/practical-machine-learning?specialization=data-science-statistics-machine-learning

Data
The training data for this project are available here:
https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv

The test data are available here:
https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv

The data for this project come from this source: http://groupware.les.inf.puc-rio.br/har. If you use the document you create for this class for any purpose please cite them as they have been very generous in allowing their data to be used for this kind of assignment.

"""

#setwd("Your Practical Machine learning directory")

#reading Data
trainDF <- read.csv("pml-training.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE, na.strings = c("","NA","#DIV/0!"))
testDF <- read.csv("pml-testing.csv", header = TRUE, sep = ",", 
                         stringsAsFactors = FALSE,
                         na.strings = c("","NA","#DIV/0!"))

#First impression of the data
names(trainDF)

#cleaning the NA and NONnecessary Columns (the first 6 columns)
col_na <- apply(trainDF, 2, function(x){any(is.na(x))})
clean_train<- trainDF[col_na!=TRUE]
clean_test<- testDF[col_na!=TRUE]
clean_train <- clean_train[-(1:7)]
clean_test <- clean_test[-(1:7)]

DF <- clean_train
library(caret)
#Understanding the data 

summary(DF)
featurePlot(x=DF[(1:10)], y=DF$classe, plot="pairs")
featurePlot(x=DF[(11:20)], y=DF$classe, plot="pairs")
# here we can easly see if "magnet_belt_z" is >-100 then the classification is always E (Walking)
featurePlot(x=DF[(21:30)], y=DF$classe, plot="pairs")

table(DF$classe)
# see if there is near Zero Variance
nearZeroVar(DF, saveMetrics=TRUE)

#see correlation 
M<-abs(cor(DF[-53]))
diag(M)<-0
which(M>0.9, arr.ind=T)


#### making the prediction STEPS
#taking numeric predictor and create preProcess object
w <- which(lapply(DF, class) %in% "numeric") 
preObj <- preProcess(DF[,w], method = c('knnImpute', 'center', 'scale'))

train_pc <- predict(preObj, clean_train[,w])
test_pc <- predict(preObj, clean_test[,w])

#Adding Classe column
train_pc$classe <- DF$classe

#eliminate NearZeroVariance
nzv <-nearZeroVar(train_pc, saveMetrics= TRUE)
train_nzv <- train_pc[, nzv$nzv==FALSE]

nzv <-nearZeroVar(test_pc, saveMetrics= TRUE)
test_nzv <- test_pc[, nzv$nzv==FALSE]

set.seed(123)
library(randomForest)


#Tree Classification and plot with Rattle 
library("rattle")
modFit <- train(classe~.,method="rpart",data=train_nvz)
fancyRpartPlot(modFit$finalModel)

Pred <- predict(modFit, clean_test)
confusionMatrix(cvPred, as.factor(crossValidation$classe))


#cross validation
inTrain <- createDataPartition(y = train_nzv$classe, p = 2/3, list = FALSE)
trainingSet <- train_nzv[inTrain,]
crossValidation <- train_nzv[-inTrain,]

modFitCV <- train(classe ~ ., data = trainingSet, method = "rf", 
                  trControl=trainControl(method='cv'))

#cross validation accuracy 
cvPred <- predict(modFitCV, crossValidation)
confusionMatrix(cvPred, as.factor(crossValidation$classe))


### Making prediction
predict(modFitCV, test_nzv)
