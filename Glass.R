library(caret)
library(plyr)
fp = "/home/admin123/MLExperiments/KernelRegression/glass.csv"
df = read.csv(fp)
df[,"GT"] = as.factor(df[,"GT"])
# recoding the class variable as svm implementation complains about validity of var name
lGT = levels(df$GT)
rGT = paste(rep("C",6), lGT, sep="_")
df$GT = mapvalues(df$GT, from = lGT, to = rGT)
#shuffle the dataset
df = df[sample.int(nrow(df)),]
set.seed(825)
test.ids = sample(0.3*nrow(df), replace = FALSE)
trng.df = df[-test.ids, ]
test.df = df[test.ids,]

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10,
                           ## Estimate class probabilities
                           classProbs = TRUE,
                           ## Evaluate performance using 
                           ## the following function
                           summaryFunction = defaultSummary)

svmFit <- train(GT ~ ., data = trng.df,
                method = "svmRadial",
                trControl = fitControl,
                preProc = c("center", "scale"),
                tuneLength = 8,
                metric = "ROC")
# Select the best model
svm.bm = svmFit$finalModel
yp = predict(svm.bm, test.df[,1:9])
confusionMatrix(yp, test.df[,10])