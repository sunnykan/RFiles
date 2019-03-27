source("load-libraries.R")
set.seed(1)

x <- matrix(rnorm(20 * 2), ncol = 2)
x
y <-  c(rep(-1, 10), rep(1, 10))
y
x[y == 1, ] <-  x[y == 1, ] + 1
plot(x, col = (3 - y))

dat <- data.frame(x = x, y = as.factor(y))
datx <- as_tibble(dat)
datx
svmfit <- svm(y ~., data = datx, kernel = "linear", cost = 10, scale = FALSE)
plot(svmfit, datx)
names(svmfit)
svmfit$index

#Lower cost = tolerate more violation (less variance, more bias)
svmfit <- svm(y ~., data = datx, kernel = "linear", cost = 0.1, scale = FALSE)
plot(svmfit, dat)
svmfit$index

#cross validation
set.seed(1)
tune.out <- tune(svm, y ~., data = datx, kernel = "polynomial",
                 ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
# examine cross validation errors
summary(tune.out)
names(tune.out)
bestmod <- tune.out$best.model
bestmod
summary(bestmod)

# generate test data
xtest <- matrix(rnorm(20 * 2), ncol = 2)
ytest <- sample(c(-1, 1), 20, replace = TRUE)
xtest[ytest == 1, ] <-  xtest[ytest == 1, ] + 1
test.dat <-  data.frame(x = xtest, y = as.factor(ytest))
test.datx <-  as_tibble(test.dat)

# make predictions using the bestmod obtained through cross validation
ypred <- predict(bestmod, test.datx)
table(predict = ypred, truth = test.datx$y)

# predict using a different cost
svmfit <- svm(y ~., data = datx, kernel = "linear", cost = 0.01, scale = FALSE)
ypred <- predict(svmfit, test.datx)
table(predict = ypred, truth = test.datx$y)

# consider two classes that are linearly separable
x[y == 1, ] <- x[y == 1, ] + 0.5
#plot(x, col = (y + 5)/2, pch = 19)
#plot(col = (y + 5)/2, x, pch = 19)

datx <- as_tibble(data.frame(x = x, y = as.factor(y)))
plot(datx$x.2, dat$x.1, col = (y + 5)/2, pch = 19)
svmfit <- svm(y ~., data = datx, kernel = "linear", cost = 1e5)
summary(svmfit)
plot(svmfit, datx)

# same model with smaller value of cost (= more tolerance for mistakes)
svmfit <- svm(y ~., data = datx, kernel = "linear", cost = 1)
summary(svmfit)
plot(svmfit, datx)

## fit svm with nonlinear kernel
# generate data

set.seed(1)
x <- matrix(rnorm(200 * 2), ncol = 2)
x[1:100, ] <- x[1:100, ] + 2
x[101:150, ] <- x[101:150, ] - 2
y <- c(rep(1, 150), rep(2, 50))
dat <- data.frame(x = x, y = as.factor(y))
datx <- as_tibble(dat)
datx
plot(datx$x.1, datx$x.2, col = (y + 4)/2, pch = 19)

# create training set
train <- sample(200, 100)
svmfit <- svm(y ~., data = datx, kernel = "radial", 
              gamma = 1, cost = 1)
plot(svmfit, dat[train,])

# increasing cost -> increasing variance = lower tolerance for mistakes
svmfit <- svm(y ~., data = datx[train, ], kernel = "radial", 
              gamma = 1, cost = 1e5)
plot(svmfit, dat[train,])

# choose gamma using cross validation
set.seed(1)
tune.out <- tune(svm, y ~., data = datx[train,],
                 kernel = "radial", 
                 ranges = list(cost = c(0.1, 1, 10, 100, 1000),
                 gamma = c(0.5, 1, 2, 3, 4)))
summary(tune.out)

# predict on test data using parameters chosen by cross validation
table(true = dat[-train, "y"], 
                 pred = predict(tune.out$best.model, newdata = datx[-train,]))

# plot ROC curve
rocplot <- function(pred, truth, ...){
    predob <- prediction(pred, truth)
    perf <- performance(predob, "tpr", "fpr")
    plot(perf, ...)
} #end of function

svmfit.opt <- svm(y ~., data = datx[train,], kernel = "radial",
                  gamma = 2, cost = 1, decision.values = TRUE)
fitted <- attributes(predict(svmfit.opt, datx[train,],
                             decision.values = TRUE))$decision.values
par(mfrow = c(1, 2))
rocplot(fitted, datx[train, "y"], main = "Training Data")

# change gamma
svmfit.flex <- svm(y ~., data = datx[train,], kernel = "radial",
                  gamma = 50, cost = 1, decision.values = TRUE)
fitted <- attributes(predict(svmfit.flex, datx[train,],
                             decision.values = TRUE))$decision.values
rocplot(fitted, datx[train, "y"], add = TRUE, col = "red")


# on test data
fitted <- attributes(predict(svmfit.opt, dat[-train,], 
                             decision.values = TRUE))$decision.values
rocplot(fitted, dat[-train, "y"], main = "Test Data")

fitted <- attributes(predict(svmfit.flex, dat[-train,], 
                             decision.values = TRUE))$decision.values
rocplot(fitted, dat[-train, "y"], add = TRUE, col = "red")

## SVM with multiple classes
set.seed(1)
x <- rbind(x, matrix(rnorm(50 * 2), ncol = 2))
y <- c(y, rep(0, 50))
x[y == 0, 2] = x[y == 0, 2] + 2
datx = as_tibble(data.frame(x = x, y = as.factor(y)))
par(mfrow = c(1, 1))
plot(x, col = (y + 1))

svmfit <- svm(y ~., data = datx, kernel = "radial", cost = 10, gamma = 1)
plot(svmfit, datx)

## Khan dataset

class(Khan)
names(Khan)

datx <- as_tibble(data.frame(x = Khan$xtrain, y = as.factor(Khan$ytrain)))
out <- svm(y ~., data = datx, kernel = "linear", cost = 10)
summary(out)

table(out$fitted, datx$y)

## on test data
datx.test <- as_tibble(data.frame(x = Khan$xtest, y = as.factor(Khan$ytest)))
pred.test <- predict(out, newdata = datx.test)
table(pred.test, datx.test$y)

## Applied ##########################
# Exercise 4
set.seed(1)
x <- matrix(rnorm(200 * 2), ncol = 2)
x[1:100, ] <- x[1:100, ] + 2
x[101:150, ] <- x[101:150, ] - 2
y <- c(rep(-1, 150), rep(1, 50))
# plot(x[,1], x[,2], col = y + 2)
qplot(x[,2], x[,1], col = y + 2)

train <- sample(200, 100)
test <- -(train)
#qplot(x[train,1], x[train,2], col = y[train] + 2)
#qplot(x[test,1], x[test,2], col = y[test] + 2)

datx <- as_tibble(data.frame(x = x, y = as.factor(y)))
datx
## fit polynomial kernel
# cost = 1, deg = 2
svm.poly <- svm(y ~., data = datx, kernel = "polynomial", cost = 1, degree = 2)
plot(svm.poly, datx)
table(svm.poly$fitted, datx$y)
svm.poly
summary(svm.poly)

# choose cost using cross validation
set.seed(1)
tune.out <- tune(svm, y ~., data = datx[train,], kernel = "polynomial",
                 ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100), 
                               degree = c(1, 2, 3)))
summary(tune.out)

# predict on test data using parameters chosen by cross validation
# cost = 10, deg = 2
pred = predict(tune.out$best.model, newdata = datx[-train,])
true = datx$y[-train]
table(pred, true) # 90%

## fit radial kernel 
svm.rad <- svm(y ~., data = datx[train,], kernel = "radial", cost = 2, 
                gamma = 2)
plot(svm.rad, datx[train,])
table(svm.rad$fitted, datx$y[train])
svm.rad
summary(svm.rad) #90%

## choose gamma and cost using cross validation
set.seed(1)
tune.out <- tune(svm, y ~., data = datx[train,],
                 kernel = "radial", 
                 ranges = list(cost = c(0.1, 1, 10, 100, 1000),
                               gamma = c(0.5, 1, 2, 3, 4)))
summary(tune.out)

# predict on test data using parameters chosen by cross validation
# cost = 1, gamma = 2
pred = predict(tune.out$best.model, newdata = datx[-train,])
true = datx$y[-train]
table(pred, true) # 90%

## fit support vector classifier
svm.lnr <- svm(y ~., data = datx, kernel = "linear", cost = 10, scale = FALSE)
plot(svm.lnr, datx)
table(svm.lnr$fitted, datx$y)
svm.lnr
summary(svm.lnr) #baseline rate for -1

## find best cost with cross-validation
set.seed(1)
tune.out <- tune(svm, y ~., data = datx[train,],
                 kernel = "linear", 
                 ranges = list(cost = c(0.1, 1, 10, 100, 1000)))
summary(tune.out)

# predict on test data using parameters chosen by cross validation
# cost = 0.1
pred = predict(tune.out$best.model, newdata = datx[-train,])
true = datx$y[-train]
table(pred, true) # 77%

## Exercise 5
# generate data set with quadratic decision boundary
x1 <- runif(500) - 0.5
x2 <- runif(500) - 0.5
y <- 1 * (x1^2 - x2^2 > 0)
plot(1 * (x1^2 - x2^2), y)

plot(x1, x2, col = y + 1)
plot(x1, x2, col = y + 2)

datx <- as_tibble(data.frame(x1 = x1, x2 = x2, y = as.factor(y)))
# Fit a logistic regression
glm.fit <- glm(y ~., data = datx, family = binomial)
tidy(glm.fit)
glance(glm.fit)

set.seed(1)
train <- sample(500, 250)
pred.train <- predict(glm.fit, newdata = datx[train,], type = "response")
pred.class <- if_else(pred.train > 0.5, 1, 0)
table(pred.class)
table(pred.class, datx$y[train]) #0.516 on training set

ggplot(data = datx[train,], aes(x = x1, y = x2, color = pred.class)) +
    geom_point()

glm.fit <- glm(y ~ x1 + x2 + x1:x2, data = datx, family = binomial)
tidy(glm.fit)
pred.train <- predict(glm.fit, newdata = datx[train,], type = "response")
pred.class <- if_else(pred.train > 0.5, 1, 0)
table(pred.class, datx$y[train])
ggplot(data = datx[train,], aes(x = x1, y = x2, color = pred.class)) +
    geom_point()

glm.fit <- glm(y ~ x1 + x2 + I(x1^2) + I(x2^2), data = datx, family = binomial)
tidy(glm.fit)
pred.train <- predict(glm.fit, newdata = datx[train,], type = "response")
pred.class <- if_else(pred.train > 0.5, 1, 0)
table(pred.class, datx$y[train]) #100%
ggplot(data = datx[train,], aes(x = x1, y = x2, color = pred.class)) +
    geom_point()

# on test data
pred.test <- predict(glm.fit, newdata = datx[test,], type = "response")
pred.class <- if_else(pred.test > 0.5, 1, 0)
table(pred.class, datx$y[test]) #100%
ggplot(data = datx[test,], aes(x = x1, y = x2, color = pred.class)) +
    geom_point()

## Fit SVM with polynomial kernel
## fit polynomial kernel
# cost = 1, deg = 2
svm.poly <- svm(y ~., data = datx, kernel = "polynomial", cost = 1, degree = 2)
plot(svm.poly, datx)
table(svm.poly$fitted, datx$y) # 0.968%
svm.poly
summary(svm.poly)

## try cross validation
set.seed(1)
tune.out <- tune(svm, y ~., data = datx, kernel = "polynomial",
                 ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100), 
                               degree = c(1, 2, 3)))
summary(tune.out)

# predict on train data using parameters chosen by cross validation
# cost = 100, deg = 2
pred = predict(tune.out$best.model, newdata = datx)
true = datx$y
table(pred, true) # 99%

# predict on test data using parameters chosen by cross validation
# cost = 100, deg = 2
pred = predict(tune.out$best.model, newdata = datx[test,])
true = datx$y[test]
table(pred, true) # 99%

## Support Vector Classifier

svm.svc <- svm(y ~., data = datx, kernel = "linear", cost = 10)
plot(svm.svc, datx)
table(svm.svc$fitted, datx$y) # 0.39%
svm.svc
summary(svm.svc)

## try cross validation
set.seed(1)
tune.out <- tune(svm, y ~., data = datx, kernel = "linear",
                 ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
summary(tune.out)

# fit on train set at best value
svm.svc <- svm(y ~., data = datx[train,], kernel = "linear", cost = 1)
plot(svm.svc, datx[train,])
table(svm.svc$fitted, datx$y[train]) # 0.348
svm.svc
summary(svm.svc) 

## Exercise 6
set.seed(10)
x <- matrix(rnorm(400 * 2), ncol = 2)
x[1:200, ] <- x[1:200, ] + 2.2
x[201:400, ] <- x[201:400, ] - 1.6
y <- c(rep(-1, 200), rep(1, 200))
# plot(x[,1], x[,2], col = y + 2)
qplot(x[,2], x[,1], col = y + 2)

datx <- as_tibble(data.frame(x1 = x[,1], x2 = x[,2], y = as.factor(y)))
ggplot(data = datx, aes(x = x1, y = x2)) + geom_point(aes(color = y))

set.seed(1)
# support vector classifier
tune.out <- tune(svm, y ~., data = datx, kernel = "linear",
                 ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100, 1000)))
summary(tune.out)

# make predictions using the bestmod obtained through cross validation
bestmod <- tune.out$best.model
ypred <- predict(bestmod, datx)
conf.table <- table(predict = ypred, truth = datx$y)
misclass <- conf.table[2] + conf.table[3]
misclass
error <- (conf.table[2] + conf.table[3])/length(datx$y)
error # 0.005
conf.table

# predict using a different cost
svm.costs.fit <- function(c, dat) {
    svmfit <- svm(y ~., data = dat, kernel = "linear", cost = c, scale = FALSE)
    ypred <- predict(svmfit, datx)
    conf.table <- table(predict = ypred, truth = datx$y)
    misclass <- conf.table[2] + conf.table[3]
    misclass
    #error <- (conf.table[2] + conf.table[3])/length(datx$y)
    #error
} # end of function

cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100, 1000)
sapply(cost, svm.costs.fit, dat = datx) 
# fitting on training set returns a lower cost = more permissive model. 
# cv returns a more restrictive (higher cost) model.

# generate test data

set.seed(5)
x <- matrix(rnorm(400 * 2), ncol = 2)
x[1:200, ] <- x[1:200, ] + 2.2
x[201:400, ] <- x[201:400, ] - 1.6
y <- c(rep(-1, 200), rep(1, 200))
# plot(x[,1], x[,2], col = y + 2)
qplot(x[,2], x[,1], col = y + 2)

datx.test <- as_tibble(data.frame(x1 = x[,1], x2 = x[,2], y = as.factor(y)))
ggplot(data = datx, aes(x = x1, y = x2)) + geom_point(aes(color = y))
sapply(cost, svm.costs.fit, dat = datx.test)
# on test data a more permissive low cost model is preferred. This is more likely
# to misclassify

########
# Exercise 7
rm(list = ls())
auto <- as_tibble(Auto)
autox <- auto %>% 
    mutate(mpgx = as.factor(if_else(mpg > median(mpg), 1, 0))) %>%
    select(-name)

glimpse(autox)
table(autox$mpgx)

## cross validate Support Vector Classifier using different costs
set.seed(1)
tune.out <- tune(svm, mpgx ~. -mpg, data = autox, kernel = "linear",
                 ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100, 1000)))
summary(tune.out)

# make predictions using the bestmod obtained through cross validation
bestmod <- tune.out$best.model
ypred <- predict(bestmod, autox)
conf.table <- table(predict = ypred, truth = autox$mpgx)
misclass <- conf.table[2] + conf.table[3]
misclass
error <- (conf.table[2] + conf.table[3])/length(autox$mpgx)
error #0.02806122
conf.table

# plots

## cross validate Support Vector Machine using radial and polynomial basis 
## kernels with different values of cost, gamma and degree.

# radial

set.seed(1)
tune.out <- tune(svm, mpgx ~. -mpg, data = autox, kernel = "radial",
                 ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100, 1000),
                               gamma = c(1e-3, 1e-2, 1e-1)))
summary(tune.out)

# as gamma increases fit becomes more non-linear (more flexible)

# make predictions using the bestmod obtained through cross validation
bestmod <- tune.out$best.model
ypred <- predict(bestmod, autox)
conf.table <- table(predict = ypred, truth = autox$mpgx)
misclass <- conf.table[2] + conf.table[3]
misclass
error <- (conf.table[2] + conf.table[3])/length(autox$mpgx)
error #0.01020408
conf.table

# polynomial

set.seed(1)
tune.out <- tune(svm, mpgx ~. -mpg, data = autox, kernel = "polynomial",
                 ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100, 1000),
                               degree = c(1, 2, 3)))
summary(tune.out)

# make predictions using the bestmod obtained through cross validation
bestmod <- tune.out$best.model
ypred <- predict(bestmod, autox)
conf.table <- table(predict = ypred, truth = autox$mpgx)
misclass <- conf.table[2] + conf.table[3]
misclass
error <- (conf.table[2] + conf.table[3])/length(autox$mpgx)
error #0.0561
conf.table



###
rm(list = ls())
auto <- as_tibble(Auto)
autox <- auto %>% 
    mutate(mpgx = as.factor(if_else(mpg > median(mpg), 1, 0)),
           cylinders = as.factor(cylinders)) %>%
    select(mpgx, weight, displacement, cylinders)

## --
p <- ggplot(autox, aes(weight, displacement)) + geom_point()
p + facet_grid(cols = vars(cylinders))
p + facet_grid(vars(mpgx), vars(cylinders))

mt <- ggplot(autox, aes(weight, displacement))
mt <- mt + geom_point(aes(color = mpgx))
mt + facet_grid(cols = vars(cylinders))
## --

svm.fit <- svm(mpgx ~., data = autox, kernel = "linear", cost = 1)

set.seed(1)
tune.out <- tune(svm, mpgx ~., data = autox, kernel = "linear",
                 ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100, 1000)))
summary(tune.out)
svm.fit <- svm(mpgx ~., data = autox, kernel = "linear", cost = 1)
plot(svm.fit, autox, displacement ~ weight, slice = list(cylinders = 6))                               

## ---
rm(list = ls())
auto <- as_tibble(Auto)
autox <- auto %>% 
    mutate(mpgx = as.factor(if_else(mpg > median(mpg), 1, 0))) %>% 
    select(-mpg)

set.seed(1)
tune.out <- tune(svm, mpgx ~., data = autox, kernel = "linear",
                 ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100, 1000)))
tune.out

svm.fit <- svm(mpgx ~., data = autox, kernel = "linear", cost = 1000)

autox %>%
    select(-name, -mpgx, -year, -origin) %>% 
    summarise(mean(all))

plot(svm.fit, autox, displacement ~ weight,
     slice = list(origin = 1, year = 73, cylinders = 8,
                  horsepower = 104, acceleration = 15.5))

plot(svm.fit, autox, displacement ~ weight,
     slice = list(origin = 1, year = 73, cylinders = 6,
                  horsepower = 104, acceleration = 15.5))
plot(svm.fit, autox, displacement ~ weight,
     slice = list(origin = 1, year = 73, cylinders = 5,
                  horsepower = 104, acceleration = 15.5))

plot(svm.fit, autox, displacement ~ weight,
     slice = list(origin = 1, year = 73, cylinders = 4,
                  horsepower = 104, acceleration = 15.5))

plot(svm.fit, autox, displacement ~ weight,
     slice = list(origin = 1, year = 73, cylinders = 3,
                  horsepower = 104, acceleration = 15.5))

plot(svm.fit, autox, horsepower ~ weight,
     slice = list(origin = 1, year = 73, cylinders = 3,
                  displacement = 194, acceleration = 15.5))

plot(svm.fit, autox, horsepower ~ weight,
     slice = list(origin = 1, year = 82, cylinders = 3,
                  displacement = 194, acceleration = 15.5))
## Exercise 8
rm(list = ls())
oj <- as_tibble(OJ)
glimpse(oj)
names(oj) <- tolower(names(oj))

set.seed(1)
train <- sample(nrow(oj), 800)
test <- -(train)

## support vector classifier
svm.lnr <- svm(purchase ~., data = oj[train,], kernel = "linear", cost = 0.01)
summary(svm.lnr)

# predict on train data
ypred <- predict(svm.lnr, oj[train,])
conf.table <- table(predict = ypred, truth = oj$purchase[train])
misclass <- conf.table[2] + conf.table[3]
misclass
error <- (conf.table[2] + conf.table[3])/length(oj$purchase[train])
error #0.16625
conf.table

# predict on test data
ypred <- predict(svm.lnr, oj[test,])
conf.table <- table(predict = ypred, truth = oj$purchase[test])
misclass <- conf.table[2] + conf.table[3]
misclass
error <- (conf.table[2] + conf.table[3])/length(oj$purchase[test])
error #0.1814815
conf.table

# full data
ypred <- predict(svm.lnr, oj)
conf.table <- table(predict = ypred, truth = oj$purchase)
misclass <- conf.table[2] + conf.table[3]
misclass
error <- (conf.table[2] + conf.table[3])/length(oj$purchase)
error #0.1700935
conf.table

## use cross validation
set.seed(1)
tune.out <- tune(svm, purchase ~., data = oj[train,], kernel = "linear",
                 ranges = list(cost = c(0.01, 0.1, 1, 5, 10, 100)))
tune.out

# make predictions on training set 
# using the bestmod obtained through cross validation
bestmod <- tune.out$best.model
ypred <- predict(bestmod, oj[train,])
conf.table <- table(predict = ypred, truth = oj$purchase[train])
misclass <- conf.table[2] + conf.table[3]
misclass
error <- (conf.table[2] + conf.table[3])/length(oj$purchase[train])
error #0.16625
conf.table

# make predictions on test set 
# using the bestmod obtained through cross validation
bestmod <- tune.out$best.model
ypred <- predict(bestmod, oj[test,])
conf.table <- table(predict = ypred, truth = oj$purchase[test])
misclass <- conf.table[2] + conf.table[3]
misclass
error <- (conf.table[2] + conf.table[3])/length(oj$purchase[test])
error #0.1814815
conf.table

# make predictions on full dataset 
# using the bestmod obtained through cross validation
bestmod <- tune.out$best.model
ypred <- predict(bestmod, oj)
conf.table <- table(predict = ypred, truth = oj$purchase)
misclass <- conf.table[2] + conf.table[3]
misclass
error <- (conf.table[2] + conf.table[3])/length(oj$purchase)
error #0.1700935
conf.table

## SVM with radial kernel
# train data
svm.rad <- svm(purchase ~., data = oj[train,], kernel = "radial", cost = 0.01)
summary(svm.rad)

# predict on train data
ypred <- predict(svm.rad, oj[train,])
conf.table <- table(predict = ypred, truth = oj$purchase[train])
misclass <- conf.table[2] + conf.table[3]
misclass
error <- (conf.table[2] + conf.table[3])/length(oj$purchase[train])
error #0.3825
conf.table

# predict on test data
ypred <- predict(svm.rad, oj[test,])
conf.table <- table(predict = ypred, truth = oj$purchase[test])
misclass <- conf.table[2] + conf.table[3]
misclass
error <- (conf.table[2] + conf.table[3])/length(oj$purchase[test])
error #0.4111111
conf.table

# full data
ypred <- predict(svm.rad, oj)
conf.table <- table(predict = ypred, truth = oj$purchase)
misclass <- conf.table[2] + conf.table[3]
misclass
error <- (conf.table[2] + conf.table[3])/length(oj$purchase)
error #0.3897196
conf.table

## select cost using cross validation
## use cross validation
set.seed(1)
tune.out <- tune(svm, purchase ~., data = oj[train,], kernel = "radial",
                 ranges = list(cost = c(0.01, 0.1, 1, 5, 10, 100)))
tune.out

# make predictions on training set 
# using the bestmod obtained through cross validation
bestmod <- tune.out$best.model
ypred <- predict(bestmod, oj[train,])
conf.table <- table(predict = ypred, truth = oj$purchase[train])
misclass <- conf.table[2] + conf.table[3]
misclass
error <- (conf.table[2] + conf.table[3])/length(oj$purchase[train])
error #0.1375
conf.table

# make predictions on test set 
# using the bestmod obtained through cross validation
bestmod <- tune.out$best.model
ypred <- predict(bestmod, oj[test,])
conf.table <- table(predict = ypred, truth = oj$purchase[test])
misclass <- conf.table[2] + conf.table[3]
misclass
error <- (conf.table[2] + conf.table[3])/length(oj$purchase[test])
error #0.1814815
conf.table

# make predictions on full dataset 
# using the bestmod obtained through cross validation
bestmod <- tune.out$best.model
ypred <- predict(bestmod, oj)
conf.table <- table(predict = ypred, truth = oj$purchase)
misclass <- conf.table[2] + conf.table[3]
misclass
error <- (conf.table[2] + conf.table[3])/length(oj$purchase)
error #0.1485981
conf.table

## Using polynomial kernel

# train data
svm.poly <- svm(purchase ~., data = oj[train,], kernel = "polynomial", 
               cost = 0.01, degree = 2)
summary(svm.poly)

# predict on train data
ypred <- predict(svm.poly, oj[train,])
conf.table <- table(predict = ypred, truth = oj$purchase[train])
misclass <- conf.table[2] + conf.table[3]
misclass
error <- (conf.table[2] + conf.table[3])/length(oj$purchase[train])
error #0.3825
conf.table

# predict on test data
ypred <- predict(svm.poly, oj[test,])
conf.table <- table(predict = ypred, truth = oj$purchase[test])
misclass <- conf.table[2] + conf.table[3]
misclass
error <- (conf.table[2] + conf.table[3])/length(oj$purchase[test])
error #0.4111111
conf.table

# full data
ypred <- predict(svm.poly, oj)
conf.table <- table(predict = ypred, truth = oj$purchase)
misclass <- conf.table[2] + conf.table[3]
misclass
error <- (conf.table[2] + conf.table[3])/length(oj$purchase)
error #0.3897196
conf.table

## select cost using cross validation
## use cross validation
set.seed(1)
tune.out <- tune(svm, purchase ~., data = oj[train,], kernel = "polynomial",
                 ranges = list(cost = c(0.01, 0.1, 1, 5, 10, 100),
                               degree = c(2, 3)))
tune.out

# make predictions on training set 
# using the bestmod obtained through cross validation
bestmod <- tune.out$best.model
ypred <- predict(bestmod, oj[train,])
conf.table <- table(predict = ypred, truth = oj$purchase[train])
misclass <- conf.table[2] + conf.table[3]
misclass
error <- (conf.table[2] + conf.table[3])/length(oj$purchase[train])
error #0.14875
conf.table

# make predictions on test set 
# using the bestmod obtained through cross validation
bestmod <- tune.out$best.model
ypred <- predict(bestmod, oj[test,])
conf.table <- table(predict = ypred, truth = oj$purchase[test])
misclass <- conf.table[2] + conf.table[3]
misclass
error <- (conf.table[2] + conf.table[3])/length(oj$purchase[test])
error #0.1814815
conf.table

# make predictions on full dataset 
# using the bestmod obtained through cross validation
bestmod <- tune.out$best.model
ypred <- predict(bestmod, oj)
conf.table <- table(predict = ypred, truth = oj$purchase)
misclass <- conf.table[2] + conf.table[3]
misclass
error <- (conf.table[2] + conf.table[3])/length(oj$purchase)
error #0.1570093
conf.table
