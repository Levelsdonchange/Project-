training<-read.table("./pml-training.csv", header=TRUE, sep=",")
testing<-read.table("./pml-testing.csv",header=TRUE, sep=",")
library(caret)
library(rattle)
library(gridExtra)

#Cleaning up Training Data Set
trainingaccel<-grepl("^accel",names(training))
trainingtotal<-grepl("^total",names(training))
roll<-grepl("^roll",names(training))
pitch<-grepl("^pitch",names(training))
yaw<-grepl("^yaw",names(training))
magnet<-grepl("^magnet",names(training))
gyro<-grepl("^gyro",names(training))
acceldata<-training[ ,trainingaccel]
rolldata<-training[ ,roll]
pitchdata<-training[ ,pitch]
yawdata<-training[,yaw]
magnetdata<-training[,magnet]
gyrodata<-training[,gyro]
totaldata<-training[,trainingtotal]
trainClasse<-cbind(acceldata,rolldata,pitchdata,yawdata,magnetdata,gyrodata,totaldata,training[ ,160])
colnames(trainClasse)[53]<-'Classe'


set.seed(400)
inTrain = createDataPartition(trainClasse$Classe, p = .60)[[1]]
trainingsubset = trainClasse[ inTrain,]
testingsubset = trainClasse[-inTrain,]


set.seed(400)
modFit<-train(Classe~.,method="rpart", data=trainingsubset)
print(modFit$finalModel)


fancyRpartPlot(modFit$finalModel,cex=.5,under.cex=1,shadow.offset=0)

classepredict=predict(modFit,testingsubset)
confusionMatrix(testingsubset$Classe,classepredict)


set.seed(400)
modFit2 <- train(Classe ~ ., method="rf",trControl=trainControl(method = "cv", number = 4), data=trainingsubset)

print(modFit2)
varImp(modFit2)

classepredict2=predict(modFit2,testingsubset)
confusionMatrix(testingsubset$Classe,classepredict2)

p1<-qplot(roll_belt,yaw_belt,colour=Classe,data=trainingsubset)
p2<-qplot(roll_belt,pitch_forearm,colour=Classe,data=trainingsubset)
grid.arrange(p1,p2,ncol=2)


insamplepredict=predict(modFit2,trainingsubset)
confusionMatrix(trainingsubset$Classe,insamplepredict)


classepredict2=predict(modFit2,testingsubset)
confusionMatrix(testingsubset$Classe,classepredict2)

testinganswers=predict(modFit2, newdata=testing)
print(testinganswers)



