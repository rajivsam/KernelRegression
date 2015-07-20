# 
# headers = c("CRIM", "ZN", "INDUS", "CHAS", "NOX", "RM", "AGE",
#             "DIS", "RAD", "TAX", "PTRATIO", "B", "LSTAT", "MEDV")
library(MASS)
bdf = as.data.frame(Boston)
summary(bdf)
bdf$chas = as.factor(bdf$chas)
# set up seperate training and test datasets
test.ids = sample(seq(1:nrow(bdf)), size = 50, replace = TRUE)
test.df = bdf[test.ids, ]
trng.df = bdf[-test.ids, ]
# Try the simple linear regression to get a baseline result.
fit.lm = lm(medv ~.,  data = trng.df)
yp = predict.lm(fit.lm,test.df)
err = test.df$medv - yp
pct_err = err/test.df$medv
x = seq(1:50)
plot(x, pct_err, main = "Percent Error With Linear Regression", xlab = "point #", ylab = "Percent Error")
mean.pct.err.lm = mean(pct_err)
plot(x, yp, main = "Predictions with Linear Regression",
     xlab = "point #", ylab = "Median House Value")
points(x, test.df$medv, pch = 19)

mean.pct.err.lm = mean(pct_err)

# Lets see how support vector regression does on the same problem
library(e1071)
fit.e1071 = tune(svm, medv~., data = trng.df, 
                 ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9)))
#get the best model
best.model = fit.e1071$best.model
yp.svm = predict(best.model, test.df)
err.svm = test.df$medv - yp.svm
pct_err.svm = err.svm/test.df$medv

x = seq(1:50)
plot(x, pct_err.svm, main = "Percent Error With SVM Regression", xlab = "point #", ylab = "Percent Error")
mean.pct.err.svm = mean(pct_err.svm)

plot(x, yp, main = "Predictions with SVM Regression",
     xlab = "point #", ylab = "Median House Value")
points(x, test.df$medv, pch = 19)


