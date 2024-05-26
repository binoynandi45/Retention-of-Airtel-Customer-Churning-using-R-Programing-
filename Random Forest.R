#Data Import
setwd("C:\\Users\\aditya\\Desktop\\")
Data<-read.csv("CallData.csv")
View(Data)
#Data Cleaning
library(stringr)
colnames(Data)<-str_replace_all(colnames(Data),"[.]","")
colnames(Data)<-str_replace_all(colnames(Data),"[_]","")
library(summarytools)
dfSummary(Data)
str(Data)
Data$RetainedCode<-as.factor(Data$RetainedCode)
is.factor(Data$RetainedCode)
table(Data$RetainedCode)
colnames(Data)
Data1<-Data[,-c(1,7,13)]
#Data Partition
set.seed(123)
library(caret)
Train <- createDataPartition(Data1$RetainedCode,p=0.70,list = FALSE)
FinalTrain<-Data1[Train,]
FinalTest<-Data1[-Train,]
View(FinalTrain)
#Model
install.packages("randomForest")
library(randomForest)
set.seed(123)
colnames(Data1)
RF1<-randomForest(RetainedCode~.,data=FinalTrain,ntree = 500, 
                  mtry = 5, importance = TRUE, 
                  proximity=TRUE)

summary(RF1)
print(RF1)
attributes(RF1)
RF1$confusion
#Prediction
P1<-predict(RF1,FinalTrain)
head(P1)
head(FinalTrain$RetainedCode)
confusionMatrix(P1,FinalTrain$RetainedCode)
P2<-predict(RF1,FinalTest)
confusionMatrix(P2,FinalTest$RetainedCode)
plot(RF1)
colnames(FinalTrain)
Tune<-tuneRF(FinalTrain[,-6],FinalTrain[,6],stepFactor = 0.5,plot = TRUE,
             ntreeTry = 300,trace = TRUE,improve = 0.05)
#Nodes of Trees
hist(treesize(RF1),main="Nodes of Trees",col="red")
varImpPlot(RF1,sort = T,n.var = 5,main = "Top 10")
importance(RF1)
varUsed(RF1)#Shows occurrence of variables in the Random Forest
#partialPlot(RF1,Train, Train$ContactedNotContacted,"1")
#Single Tree from the forest
getTree(RF1,2,labelVar = TRUE)
#Proximity plot
#MDSplot(RF1,Train$RetainedCode)
