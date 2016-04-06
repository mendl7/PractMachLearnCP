library(caret)

# PractMachLearnCP
training<-read.csv("pml-training.csv")
# Activity class is in classe. 

# Remove columns with missing values
training2<-training[,which(colSums(is.na(training))==0)]
# Remove columns with zero or near zero variance
training3<-training2[,-nearZeroVar(training2)]
# Remove extraneous columns
training4<-training3[,-c(1:6)]

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





