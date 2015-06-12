library(caret)
library(reshape)

#Read in data
Training <- read.csv("train.csv")
Testing <- read.csv("test.csv")

#Clean up the data
Training$Wilderness <- 0
Training[which(Training$Wilderness_Area1 == 1), "Wilderness"] <- 1
Training[which(Training$Wilderness_Area2 == 1), "Wilderness"] <- 2
Training[which(Training$Wilderness_Area3 == 1), "Wilderness"] <- 3
Training[which(Training$Wilderness_Area4 == 1), "Wilderness"] <- 4
Training <- Training[, -grep("Wilderness_Area", names(Training))]

Training$SoilType <- 0
Training[which(Training$Soil_Type1 == 1), "SoilType"] <- 1
Training[which(Training$Soil_Type2 == 1), "SoilType"] <- 2
Training[which(Training$Soil_Type3 == 1), "SoilType"] <- 3
Training[which(Training$Soil_Type4 == 1), "SoilType"] <- 4
Training[which(Training$Soil_Type5 == 1), "SoilType"] <- 5
Training[which(Training$Soil_Type6 == 1), "SoilType"] <- 6
Training[which(Training$Soil_Type7 == 1), "SoilType"] <- 7
Training[which(Training$Soil_Type8 == 1), "SoilType"] <- 8
Training[which(Training$Soil_Type9 == 1), "SoilType"] <- 9
Training[which(Training$Soil_Type10 == 1), "SoilType"] <- 10
Training[which(Training$Soil_Type11 == 1), "SoilType"] <- 11
Training[which(Training$Soil_Type12 == 1), "SoilType"] <- 12
Training[which(Training$Soil_Type13 == 1), "SoilType"] <- 13
Training[which(Training$Soil_Type14 == 1), "SoilType"] <- 14
Training[which(Training$Soil_Type15 == 1), "SoilType"] <- 15
Training[which(Training$Soil_Type16 == 1), "SoilType"] <- 16
Training[which(Training$Soil_Type17 == 1), "SoilType"] <- 17
Training[which(Training$Soil_Type18 == 1), "SoilType"] <- 18
Training[which(Training$Soil_Type19 == 1), "SoilType"] <- 19
Training[which(Training$Soil_Type20 == 1), "SoilType"] <- 20
Training[which(Training$Soil_Type21 == 1), "SoilType"] <- 21
Training[which(Training$Soil_Type22 == 1), "SoilType"] <- 22
Training[which(Training$Soil_Type23 == 1), "SoilType"] <- 23
Training[which(Training$Soil_Type24 == 1), "SoilType"] <- 24
Training[which(Training$Soil_Type25 == 1), "SoilType"] <- 25
Training[which(Training$Soil_Type26 == 1), "SoilType"] <- 26
Training[which(Training$Soil_Type27 == 1), "SoilType"] <- 27
Training[which(Training$Soil_Type28 == 1), "SoilType"] <- 28
Training[which(Training$Soil_Type29 == 1), "SoilType"] <- 29
Training[which(Training$Soil_Type30 == 1), "SoilType"] <- 30
Training[which(Training$Soil_Type31 == 1), "SoilType"] <- 31
Training[which(Training$Soil_Type32 == 1), "SoilType"] <- 32
Training[which(Training$Soil_Type33 == 1), "SoilType"] <- 33
Training[which(Training$Soil_Type34 == 1), "SoilType"] <- 34
Training[which(Training$Soil_Type35 == 1), "SoilType"] <- 35
Training[which(Training$Soil_Type36 == 1), "SoilType"] <- 36
Training[which(Training$Soil_Type37 == 1), "SoilType"] <- 37
Training[which(Training$Soil_Type38 == 1), "SoilType"] <- 38
Training[which(Training$Soil_Type39 == 1), "SoilType"] <- 39
Training[which(Training$Soil_Type40 == 1), "SoilType"] <- 40
Training <- Training[, -grep("Soil_Type", names(Training))]

Training$Wilderness <- as.factor(Training$Wilderness)
Training$SoilType <- as.factor(Training$SoilType)
Training$Cover_Type <- as.factor(Training$Cover_Type)

names(Training)[5] <- "Hdisthydro"
names(Training)[6] <- "Vdisthydro"
names(Training)[7] <- "Hdistroad"
names(Training)[11] <- "Hdistfire"

Testing$Wilderness <- 0
Testing[which(Testing$Wilderness_Area1 == 1), "Wilderness"] <- 1
Testing[which(Testing$Wilderness_Area2 == 1), "Wilderness"] <- 2
Testing[which(Testing$Wilderness_Area3 == 1), "Wilderness"] <- 3
Testing[which(Testing$Wilderness_Area4 == 1), "Wilderness"] <- 4
Testing <- Testing[, -grep("Wilderness_Area", names(Testing))]

Testing$SoilType <- 0
Testing[which(Testing$Soil_Type1 == 1), "SoilType"] <- 1
Testing[which(Testing$Soil_Type2 == 1), "SoilType"] <- 2
Testing[which(Testing$Soil_Type3 == 1), "SoilType"] <- 3
Testing[which(Testing$Soil_Type4 == 1), "SoilType"] <- 4
Testing[which(Testing$Soil_Type5 == 1), "SoilType"] <- 5
Testing[which(Testing$Soil_Type6 == 1), "SoilType"] <- 6
Testing[which(Testing$Soil_Type7 == 1), "SoilType"] <- 7
Testing[which(Testing$Soil_Type8 == 1), "SoilType"] <- 8
Testing[which(Testing$Soil_Type9 == 1), "SoilType"] <- 9
Testing[which(Testing$Soil_Type10 == 1), "SoilType"] <- 10
Testing[which(Testing$Soil_Type11 == 1), "SoilType"] <- 11
Testing[which(Testing$Soil_Type12 == 1), "SoilType"] <- 12
Testing[which(Testing$Soil_Type13 == 1), "SoilType"] <- 13
Testing[which(Testing$Soil_Type14 == 1), "SoilType"] <- 14
Testing[which(Testing$Soil_Type15 == 1), "SoilType"] <- 15
Testing[which(Testing$Soil_Type16 == 1), "SoilType"] <- 16
Testing[which(Testing$Soil_Type17 == 1), "SoilType"] <- 17
Testing[which(Testing$Soil_Type18 == 1), "SoilType"] <- 18
Testing[which(Testing$Soil_Type19 == 1), "SoilType"] <- 19
Testing[which(Testing$Soil_Type20 == 1), "SoilType"] <- 20
Testing[which(Testing$Soil_Type21 == 1), "SoilType"] <- 21
Testing[which(Testing$Soil_Type22 == 1), "SoilType"] <- 22
Testing[which(Testing$Soil_Type23 == 1), "SoilType"] <- 23
Testing[which(Testing$Soil_Type24 == 1), "SoilType"] <- 24
Testing[which(Testing$Soil_Type25 == 1), "SoilType"] <- 25
Testing[which(Testing$Soil_Type26 == 1), "SoilType"] <- 26
Testing[which(Testing$Soil_Type27 == 1), "SoilType"] <- 27
Testing[which(Testing$Soil_Type28 == 1), "SoilType"] <- 28
Testing[which(Testing$Soil_Type29 == 1), "SoilType"] <- 29
Testing[which(Testing$Soil_Type30 == 1), "SoilType"] <- 30
Testing[which(Testing$Soil_Type31 == 1), "SoilType"] <- 31
Testing[which(Testing$Soil_Type32 == 1), "SoilType"] <- 32
Testing[which(Testing$Soil_Type33 == 1), "SoilType"] <- 33
Testing[which(Testing$Soil_Type34 == 1), "SoilType"] <- 34
Testing[which(Testing$Soil_Type35 == 1), "SoilType"] <- 35
Testing[which(Testing$Soil_Type36 == 1), "SoilType"] <- 36
Testing[which(Testing$Soil_Type37 == 1), "SoilType"] <- 37
Testing[which(Testing$Soil_Type38 == 1), "SoilType"] <- 38
Testing[which(Testing$Soil_Type39 == 1), "SoilType"] <- 39
Testing[which(Testing$Soil_Type40 == 1), "SoilType"] <- 40
Testing <- Testing[, -grep("Soil_Type", names(Testing))]

Testing$Wilderness <- as.factor(Testing$Wilderness)
Testing$SoilType <- as.factor(Testing$SoilType)

names(Testing)[5] <- "Hdisthydro"
names(Testing)[6] <- "Vdisthydro"
names(Testing)[7] <- "Hdistroad"
names(Testing)[11] <- "Hdistfire"

#Now build model
##First create 'training' and 'valid'(ation) datasets and test them out
invalid <- createDataPartition(y=Training$Cover_Type, p=0.2, list=F)
valid <- Training[invalid, ]
training <- Training[-invalid, ]

fit1 <- train(Cover_Type ~ . - Id - Aspect, data=training, method="rf")
predvalid1 <- predict(fit1, valid)
predvalidtable1 <- table(predvalid1, valid$Cover_Type)
confusionMatrix(predvalidtable1)

fit2 <- train(Cover_Type ~ . - Id - Aspect, data=training, method="gbm")
predvalid2 <- predict(fit2, valid)
predvalidtable2 <- table(predvalid2, valid$Cover_Type)
confusionMatrix(predvalidtable2)

##Now build model on full 'Training' data to use to predict on 'Testing' data
Fit1 <- train(Cover_Type ~ . - Id - Aspect, data=Training, method="rf")
TestingFit1 <- Testing[Testing$SoilType != 15 & Testing$SoilType != 7, ]
Pred1 <- predict(Fit1, TestingFit1)

Training715 <- Training[, -14]
Fit2 <- train(Cover_Type ~ . - Id - Aspect, data=Training715, method="rf")
TestingFit2 <- Testing[Testing$SoilType == 15 | Testing$SoilType == 7, ]
TestingFit2 <- TestingFit2[, -13]
Pred2 <- predict(Fit2, TestingFit2)

###caret randomForest package with sampsize controlled
weight <- c(sum(Pred1==1), sum(Pred1==2), sum(Pred1==3), sum(Pred1==4), sum(Pred1==5), sum(Pred1==6), sum(Pred1==7))
weight <- weight/length(Pred1)
sampsizes <- numeric(7)
sampsizes[1] <- floor(weight[1]*sum(Training$Cover_Type==1))
sampsizes[2] <- floor(weight[2]*sum(Training$Cover_Type==2))
sampsizes[3] <- floor(weight[3]*sum(Training$Cover_Type==3))
sampsizes[4] <- floor(weight[4]*sum(Training$Cover_Type==4))
sampsizes[5] <- floor(weight[5]*sum(Training$Cover_Type==5))
sampsizes[6] <- floor(weight[6]*sum(Training$Cover_Type==6))
sampsizes[7] <- floor(weight[7]*sum(Training$Cover_Type==7))
Fit8 <- train(Cover_Type ~ . - Id - Aspect, method="rf", data=Training, sampsize=sampsizes)
Pred8 <- predict(Fit8, TestingFit1)
###

Testing8 <- data.frame(cbind(Id=TestingFit1$Id, Cover_Type=Pred8))
Testing2 <- data.frame(cbind(Id=TestingFit2$Id, Cover_Type=Pred2))
TestingPred <- data.frame(rbind(Testing8, Testing2))
TestingPred <- TestingPred[order(TestingPred[,1]),]

write.csv(TestingPred, file = "ForestCoverResult.csv",row.names=FALSE)


