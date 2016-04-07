library(caret)

# Training data
training<-read.csv("pml-training.csv")
# Activity class is in classe. 

# Testing data
testing<-read.csv("pml-testing.csv")

# Remove columns with missing values
training2<-training[,which(colSums(is.na(training))==0)]
# Remove columns with zero or near zero variance
training3<-training2[,-nearZeroVar(training2)]
# Remove extraneous columns
training4<-training3[,-c(1:6)]

# Process testing data to contain the same columns as the 
# cleaned training data.
testing2<-testing[,names(training4[,-c(53)])]

# Cast training data variable types to match testing data
for(i in 1:dim(training4[,-c(53)])[2]){
  class(training4[,i])<-class(testing2[,i])
}

set.seed(1234)

# Split training data into cross-validated datasets for 
# model and method exploration. 
myTrain<-createDataPartition(y=training4$classe, p=0.60, list=FALSE)
mytraining<-training4[myTrain,]
mytesting<-training4[-myTrain,]



# split training data in k-folds
folds<-createFolds(y=training4$classe, k=2, list=TRUE, returnTrain=TRUE)
mytraining1<-training4[folds[[1]],]
mytest1<-training4[-folds[[1]],]

# Clean up workspace
rm(training, training2, training3)

# Do feature selection based on RF with full training.
selectFeat<-c("classe", "roll_belt", "pitch_forearm", "yaw_belt", "pitch_belt", "magnet_dumbbell_y", "magnet_dumbbell_z", "roll_forearm", "accel_dumbbell_y", "accel_forearm_x", "roll_dumbbell", "magnet_dumbbell_x", "magnet_belt_z", "accel_dumbbell_z", "magnet_forearm_z", "total_accel_dumbbell", "magnet_belt_y", "accel_belt_z", "yaw_arm", "gyros_belt_z", "magnet_belt_x", "roll_arm", "accel_forearm_z", "yaw_dumbbell", "gyros_dumbbell_y", "magnet_forearm_y", "magnet_arm_y", "yaw_forearm", "magnet_arm_x", "accel_dumbbell_x", "accel_arm_x")
mytraining1<-mytraining[,selectFeat]
mytest1<-mytesting[,selectFeat]

# Regression partition tree
modFitdev1<-train(classe~., method="rpart", data=mytraining1)
pred1<-predict(modFitdev1, newdata=mytest1)
confusionMatrix(pred1, mytesting$classe) # Accuracy:0.4904
# center and scale predictors
modFitdev1<-train(classe~., preProcess=c("center", "scale"), method="rpart", data=mytraining)
# 105 secs with selected features
pred1<-predict(modFitdev1, newdata=mytesting)
confusionMatrix(pred1, mytesting$classe) # Accuracy:0.4904 did absolutely nothing
# glm with PCA predictors
preProc<-preProcess(mytraining[,-53], method="pca", pcaComp=3)
trainPC<-predict(preProc, mytraining[,-53])
modFitdev4<-train(mytraining$classe~., method="glm", data=trainPC) # faiil 
# Try selected features. which are numeric/integer
modFitdev4<-train(mytraining1$classe~., method="glm", data=mytraining1)
# Failed because glms can only predict binary outcomes?


# Random Forest on Kfold 1 training data (didnt finish)
modFitdev2<-train(classe~., method="rf", data=mytraining1)
# boosted trees (didn't finish)
modFitdev3<-train(classe~., method="gbm", data=mytraining1, verbose=FALSE)


nsv<-nearZeroVar(training2, saveMetrics=TRUE)
nsv[which(nsv$zeroVar==TRUE|nsv$nzv==TRUE),]

summary(factor(training2[which(training2$user_name=="pedro"&training2$classe=="A"),c("num_window")]))
summary(factor(training2[which(training2$user_name=="pedro"&training2$classe=="A"),c("num_window")]))

# Exploratory analysis of predictors.
featurePlot(x=training3[,c(7:10)], y=training$classe, plot="pairs")
featurePlot(x=training3[,c(11:15)], y=training$classe, plot="pairs")
featurePlot(x=training3[,c(16:20)], y=training$classe, plot="pairs") # good examples

# Build Random forest model
modFitRF<-train(classe~., data=training4, method="rf", prox=TRUE)





