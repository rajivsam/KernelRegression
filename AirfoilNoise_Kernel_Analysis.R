library(kernlab)
fp = "/home/admin123/MLExperiments/airfoil_self_noise.csv"
afdf = read.csv(fp, header = TRUE)
afdf = afdf[complete.cases(afdf),]

library(KRLS)
# pick 300 points to develop a regression model
subset.points = sample(1:nrow(afdf), 400)
scaled.data = as.data.frame(scale(afdf))
trng.points = sample(subset.points, 300)
X = scaled.data[trng.points,1:5]
Y = scaled.data[trng.points,6]
test.points = subset.points[! subset.points %in% trng.points]
test.df = scaled.data[test.points,]
test.X = test.df[,1:5]
test.Y = test.df[,6]

fit.krls = krls(X,Y)
yp.krls = predict(fit.krls, newdata=test.X)
err.krls = yp.krls$fit  - test.Y
mse.krls = 0.01 * sqrt(abs(sum(err.krls)))

# try the kernlab svm implementation
trng.df = X
trng.df$SSP = Y
fit.ksvm = ksvm(SSP ~ ., data = trng.df, kernel.dot = "rbfdot",
                kpar=list(sigma=5), type="eps-svr")
yp.ksvm = predict(fit.ksvm, newdata=test.X)
err.ksvm = yp.ksvm - test.Y
mse.ksvm = 0.01 * sqrt(abs(sum(err.ksvm)))

# plot the fit 
pt = seq(1:100)
yp.fit.krls = yp.krls$fit
plot( pt, yp.fit.krls, main="Fitted versus Actual (KRLS)", xlab = "test point", ylab = "Pressure MPa")
points(pt, test.Y, pch=19)

plot(pt, err.krls, main="Error versus location (KRLS)", xlab = "test point", ylab = "Error")


fit.kpca = kpca(as.matrix(scaled.data), kernel = "rbfdot", kpar = list(sigma = 5))
proj.pts = predict(fit.kpca, scaled.data)
p = rotated(fit.kpca)
# The following should yield the same plots
plot(proj.pts , main = "Calculating Projections")
plot(p , main = "Rotation Matrix ")

# How well does a linear model do

fit.lm = lm(SSP ~ ., data = scaled.data)
yp.lm = predict(fit.lm, test.X)
err.lm = yp.lm - test.Y
mse.lm = 0.01 * sqrt(abs(sum(err.lm)))