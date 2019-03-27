source("load-libraries.R")
source("subsets-plots.R")

hitters <- as.tibble(Hitters)
glimpse(hitters)
names(hitters) <- tolower(names(hitters))
dim(hitters)

sum(is.na(hitters$salary))
# drop any row in dataframe with missing values 
hitters <- hitters[complete.cases(hitters),]
sum(is.na(hitters$salary))

regfit.full <- regsubsets(salary ~., hitters)
summary(regfit.full)

regfit.full <- regsubsets(salary ~., hitters, nvmax = 19)
reg.summary = summary(regfit.full)
names(reg.summary)
reg.summary$rsq
which.min(reg.summary$bic)
subsets_plots(reg.summary, 19)

par(mfrow = c(2,2))
plot(reg.summary$rss ,xlab = "Number of Variables ", ylab = "RSS",
     type = "l")
plot(reg.summary$adjr2 ,xlab = "Number of Variables ",
     ylab = "Adjusted RSq",type = "l")
points(which.max(reg.summary$adjr2),
       reg.summary$adjr2[11], col = "red", cex = 2, pch = 20)

plot(reg.summary$cp ,xlab = "Number of Variables ", ylab = "Cp", type = "l")
points(which.min(reg.summary$bic), 
       reg.summary$cp[10], col = "red", cex = 2, pch = 20)
plot(reg.summary$bic ,xlab = "Number of Variables ",ylab = "BIC", type = "l")
points(which.min(reg.summary$bic), 
       reg.summary$bic[6],col = "red",cex = 2,pch = 20)

plot(regfit.full, scale = "r2")
plot(regfit.full, scale = "adjr2")
plot(regfit.full, scale = "r2")
plot(regfit.full, scale = "bic")
coef(regfit.full, which.min(reg.summary$bic))

regfit.fwd <- regsubsets(salary ~., hitters, nvmax = 19, method = "forward")
reg.summary <- summary(regfit.fwd)
reg.summary
which.min(reg.summary$bic)
subsets_plots(reg.summary, 19)

regfit.bwd <- regsubsets(salary ~., hitters, nvmax = 19, method = "backward")
reg.summary <- summary(regfit.bwd)
reg.summary
which.min(reg.summary$bic)
subsets_plots(reg.summary, 19)

coef(regfit.full, 7)
coef(regfit.fwd, 7)
coef(regfit.bwd, 7)

# 6.5.3
set.seed(1)
train = sample(c(TRUE, FALSE), nrow(hitters), replace = TRUE)
test = !train

regfit.best <- regsubsets(salary ~., data = hitters[train,], nvmax = 19)

#create a design matrix
test.mat <- model.matrix(salary ~., data = hitters[test,])

val.errors = rep(NA, 19)

calc_rss <- function(i) {
    # extract coefficients
    coefi <- coef(regfit.best, id = i)
    # multiply design matrix by coefficients to get predictions
    pred <- test.mat[, names(coefi)] %*% coefi
    # square the residuals and take square root to get MSE
    val.errors[i] <- mean((hitters$salary[test] - pred)^2)
}

test_mse_vals <- sapply(1:19, calc_rss)
test_mse_vals[which.min(test_mse_vals)]
which.min(test_mse_vals)
coef(regfit.best, which.min(test_mse_vals))
data <- tibble(size = c(1:19), mse = test_mse_vals)
ggplot(data, aes(x = size, y = mse)) +
    geom_point(color = "green")

predict.regsubsets <- function(object, newdata, id, ...) {
    form <- as.formula(object$call[[2]])
    mat <- model.matrix(form, newdata)
    coefi <- coef(object, id = id)
    xvars <- names(coefi)
    mat[, xvars] %*% coefi #return vector of predictions
}

#use full data. select model with 10 variables
regfit.best <- regsubsets(salary ~., data = hitters, nvmax = 19)
coef(regfit.best, 10)

# USING CROSS-VALIDATION

k = 10
set.seed(1)
folds <- sample(1:k, nrow(hitters), replace = TRUE)
cv.errors <- matrix(NA, k, 19, dimnames = list(NULL, paste(1:19)))

for (j in 1:k) {
    best.fit <- regsubsets(salary ~., data = hitters[folds != j,], nvmax = 19)
    for (i in 1:19) {
        pred = predict.regsubsets(best.fit, hitters[folds == j,], id = i)
        cv.errors[j, i] <- mean((hitters$salary[folds == j] - pred)^2)
    } # end of inside for loop
} # end of outside for loop

# average columns to get average mse across folds (k) by number of variables (i)
mean.cv.errors = apply(cv.errors, 2, mean)
plot(mean.cv.errors, type = 'b')
reg.best = regsubsets(salary ~., data = hitters, nvmax = 19)
coef(reg.best, 11) # 11 is obtained from cross validation

# 6.6 Ridge regression and lasso

rm(list = ls())
hitters <- as.tibble(Hitters)
glimpse(hitters)
names(hitters) <- tolower(names(hitters))
dim(hitters)

sum(is.na(hitters$salary))
# drop any row in dataframe with missing values 
hitters <- hitters[complete.cases(hitters),]
sum(is.na(hitters$salary))

# create design matrix
x = model.matrix(salary ~., hitters)[,-1] #removed the column with 1s (ones)
y = hitters$salary
# RIDGE alpha = 0
grid <- 10^seq(10, -2, length = 100)
ridge.mod = glmnet(x, y, alpha = 0, lambda = grid)

# lambda = 11498
ridge.mod$lambda[50]
coef(ridge.mod)[,50]
#l2 norm
sqrt(sum(coef(ridge.mod)[-1, 50]^2))

# lambda = 705 (smaller), coefficients bigger
ridge.mod$lambda[60]
coef(ridge.mod)[,60]
#l2 norm
sqrt(sum(coef(ridge.mod)[-1, 60]^2))

# obtain ridge regression coefficients for a new value of lambda = 50
predict(ridge.mod, s = 50, type = "coefficients")

# Use train and test set

set.seed(1)
train <- sample(1:nrow(x), nrow(x)/2)
test <- (-train)
y.test <- y[test]

# ridge on training set
ridge.mod <-  glmnet(x[train,], y[train], 
                   alpha = 0, lambda = grid, thresh = 1e-12)
# get predictions for test set
ridge.pred <- predict(ridge.mod, s = 4, newx = x[test, ])
mean((ridge.pred - y.test)^2)

# for an Intercept-only model. Use the mean of the training observations
mean((mean(y[train]) - y.test)^2)
# or use a very high Lambda value - drive coefficients to zero
ridge.pred = predict(ridge.mod, s = 1e10, newx = x[test,])
mean((ridge.pred - y.test)^2)

# lamda = 0 -> Least Squares results
ridge.pred <- predict(ridge.mod, 
                      s = 0, 
                      x = x[train,],
                      y = y[train],
                      newx = x[test,],
                      exact = TRUE)

mean((ridge.pred - y.test)^2)

# compare with least squares
lm(y ~ x, subset = train)
predict(ridge.mod, 
        s = 0, 
        x = x[train,],
        y = y[train],
        exact = T, 
        type = "coefficients")[1:20,]

# Using built-in cross validation function to select best lambda
set.seed(1)
cv.out <- cv.glmnet(x[train, ], y[train], alpha = 0)
plot(cv.out)
bestlam = cv.out$lambda.min
bestlam

# test MSE for lambda = bestlam
ridge.pred <- predict(ridge.mod, s = bestlam, newx = x[test, ])
mean((ridge.pred - y.test)^2)

# fit to full data set using lambda = bestlam
out <- glmnet(x, y, alpha = 0)
predict(out, type = "coefficients", s = bestlam)[1:20,]

# LASSO
lasso.mod <- glmnet(x[train,], y[train], alpha = 1, lambda = grid)
plot(lasso.mod)

set.seed(1)
cv.out <- cv.glmnet(x[train,], y[train], alpha = 1)
plot(cv.out)
bestlam <- cv.out$lambda.min
lasso.pred <- predict(lasso.mod, s = bestlam, newx = x[test,])
mean((lasso.pred - y.test)^2)

# Fit to full dataset
out <- glmnet(x, y, alpha = 1, lambda = grid)
lasso.coef <- predict(out, type = "coefficients", s = bestlam)[1:20,]
lasso.coef
plot(out)

### 6.7.1 PCR and PLS
rm(list = ls())

hitters <- as.tibble(Hitters)
glimpse(hitters)
names(hitters) <- tolower(names(hitters))
dim(hitters)

sum(is.na(hitters$salary))
# drop any row in dataframe with missing values 
hitters <- hitters[complete.cases(hitters),]
sum(is.na(hitters$salary))

x = model.matrix(salary ~., hitters)[,-1] #removed the column with 1s (ones)
y = hitters$salary

set.seed(1)
train <- sample(1:nrow(x), nrow(x)/2)
test <- (-train)
y.test <- y[test]

set.seed(2)
pcr.fit <- pcr(salary ~., data = hitters, scale = TRUE, validation = "CV")
summary(pcr.fit)
validationplot(pcr.fit, val.type = "MSEP")

# use train data
set.seed(1)
pcr.fit <- pcr(salary ~., data = hitters, 
               scale = TRUE, validation = "CV",
               subset = train)
validationplot(pcr.fit, val.type = "MSEP")

# predict using test data (M = 7, minimum MSEP)
pcr.pred <- predict(pcr.fit, x[test,], ncomp = 7)
mean((pcr.pred - y.test)^2)

# fit on full model using M = 7
pcr.fit <- pcr(y ~ x, scale = TRUE, ncomp = 7)
summary(pcr.fit)

# PLS: Partial Least Squares
set.seed(1)
pls.fit <- plsr(salary ~., data = hitters, subset = train, 
                scale = TRUE,
                validation = "CV")
summary(pls.fit)
validationplot(pls.fit, val.type = "MSEP")

# evaluate on test set with M = 2
pls.pred <- predict(pls.fit, x[test, ], ncomp = 2)
mean((pls.pred - y.test)^2)

# fit on full dataset with M = 2
pls.fit <- plsr(salary ~., data = hitters, scale = TRUE, ncomp = 2)
summary(pls.fit)


### Exercises: Applied
rm(list = ls())

# 8
set.seed(1)
X <- rnorm(100)
e <- rnorm(100)
#Y <- 0.5 + 3 * X + 1.5 * (X^2) - 2.0 * (X^3) + e
b0 <- 2
b1 <- 3
b2 <- -1
b3 <- -0.5
Y <- b0 + b1 * X + b2 * (X^2) + b3 * (X^3) + e

data <- tibble(X, Y)

#SUBSETS
regfit.full <- regsubsets(Y ~ poly(X, 10), data, nvmax = 10)
reg.summary <- summary(regfit.full)

#USING ggplot
nvars <- regfit.full$np - 1 #number of variables 
source("subsets-plots.R")
subsets_plots(reg.summary, nvars)
coef(regfit.full, 3)

# forward and backward
regfit.fwd <- regsubsets(Y ~ poly(X, 10), data, nvmax = 10, method = "forward")


reg.fwd.summary <- summary(regfit.fwd)
nvars <- regfit.fwd$np - 1
subsets_plots(reg.fwd.summary, nvars)
coef(regfit.fwd, 3)

regfit.bwd <- regsubsets(Y ~ poly(X, 10), data, nvmax = 10, method = "backward")
reg.bwd.summary <- summary(regfit.bwd)
nvars <- regfit.bwd$np - 1
subsets_plots(reg.bwd.summary, nvars)
coef(regfit.bwd, 3)

#LASSO
grid <- 10^seq(10, -2, length = 100)
x = model.matrix(Y ~ poly(X, 10), data = data)[,-1] #removed the column with 1s (ones)
y = Y

fit_lasso <- glmnet(x, y, alpha = 1)
plot(fit_lasso)
cv_lasso <- cv.glmnet(x, y, alpha = 1)
plot(cv_lasso)
bestlam <- cv_lasso$lambda.min
predict(fit_lasso, s = bestlam, type = "coefficients")[1:11,]

set.seed(1)
x <- rnorm(100)
e <- rnorm(100)
b7 <- 0.8
# Y <- b0 + b7 * I(X^7) + e
# plot(X, Y)

data <- tibble(x = X, y = Y)

y <- b0 + b7 * x^7 + e
plot(x, y)
data.full <- data.frame(y = y, x = x)
regfit.full <- regsubsets(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10), data = data.full, nvmax = 10)

#SUBSETS
#regfit.full <- regsubsets(y ~ poly(x, 10), data, nvmax = 10)
reg.summary <- summary(regfit.full)

#USING ggplot
nvars <- regfit.full$np - 1 #number of variables 
source("subsets-plots.R")
subsets_plots(reg.summary, nvars)
coef(regfit.full, 1)

# LASSO
x = model.matrix(Y ~ poly(X, 10, raw = TRUE), data = data)[,-1]
#x = model.matrix(Y ~ poly(X, 10, raw = FALSE), data = data)[,-1]

fit_lasso <- glmnet(x, y, alpha = 1)
plot(fit_lasso)
cv_lasso <- cv.glmnet(x, y, alpha = 1)
plot(cv_lasso)
bestlam <- cv_lasso$lambda.min
predict(fit.lasso, s = bestlam, type = "coefficients")[1:11, ]

# EXAMPLE 9
rm(list = ls())
set.seed(11)

college <- as.tibble(College)
glimpse(college)
college <- college[complete.cases(college),]
dim(college)
names(college) <- tolower(names(college))

#train = sample(c(TRUE, FALSE), nrow(college), replace = TRUE)
#test = !train
#table(train, test)

train = sample(1:dim(College)[1], dim(College)[1] / 2)
test <- -train


# linear model
lm.model <- lm(apps ~., data = college, subset = train)
apps.pred <- predict(lm.model, newdata = college[test,])
lm_rmse <- mean((apps.pred - college$apps[test])^2)
lm_rmse

# ridge
X <- model.matrix(apps ~., data = college)[,-1]
Y <- college$apps

grid <- 10^seq(10, -2, length = 100)
#ridge.mod <- glmnet(X[train,], Y[train], 
#                    alpha = 0, lambda = grid, thresh = 1e-12)
cv.out <- cv.glmnet(X[train, ], Y[train], alpha = 0, 
                    lambda = grid, thresh = 1e-12)
plot(cv.out)
bestlam <- cv.out$lambda.min
test.pred <- predict(cv.out, newx = X[test,], s = bestlam)

ridge_rmse <- mean((test.pred - Y[test])^2)
ridge_rmse

ridge.full.mod <- glmnet(X, Y, alpha = 0, lambda = grid, thresh = 1e-12)
predict(ridge.full.mod, s = bestlam, type = "coefficients")

# lasso
cv.out <- cv.glmnet(X[train, ], Y[train], alpha = 1, 
                    lambda = grid, thresh = 1e-12)
plot(cv.out)
bestlam <- cv.out$lambda.min
test.pred <- predict(cv.out, newx = X[test,], s = bestlam)
lasso_test_rmse <- mean((test.pred - Y[test])^2)
lasso_test_rmse

out = glmnet(X, Y, alpha = 1)
predict(out, type = "coefficients", s = bestlam)

#PCR
pcr.fit <- pcr(apps ~., data = college,
               scale = TRUE, validation = "CV",
               subset = train)
validationplot(pcr.fit, val.type = "MSEP")

# predict using test data (M = 10, minimum MSEP)
pcr.pred <- predict(pcr.fit, X[test,], ncomp = 10)
pcr_rmse <- mean((pcr.pred - Y[test])^2)
pcr_rmse

# fit on full model using M = 7
pcr.fit <- pcr(Y ~ X, scale = TRUE, ncomp = 10)
summary(pcr.fit)

# PLS
pls.fit <- plsr(apps ~., data = college, subset = train, 
                scale = TRUE,
                validation = "CV")
summary(pls.fit)
validationplot(pls.fit, val.type = "MSEP")

# evaluate on test set with M = 10
pls.pred <- predict(pls.fit, X[test, ], ncomp = 10)
plsr_rmse <- mean((pls.pred - Y[test])^2)

# fit on full dataset with M = 10
pls.fit <- plsr(Y ~ X, scale = TRUE, ncomp = 10)
summary(pls.fit)

# Example 10
set.seed(1)
X <- as.matrix(sapply(1:20, function(x) rnorm(1000)))
# matrix(rnorm(1000 * 20), 1000, 20)
dimnames(X) <- list(NULL, paste("x_", 1:20, sep = ""))
e <- rnorm(1000)
betas <- rnorm(20)
betas[c(3, 4, 9, 10, 19)] <- 0 #set some betas to zero
names(betas) <- paste("x_", 1:20, sep = "")

Y <- X %*% betas + e
hist(Y)

data_xy <- as.tibble(x = X)
data_xy["y"] <- Y
head(data_xy)

train <- sample(c(1:1000), 100)
test <- -(train)

#subsets
train.fit <- regsubsets(y ~., data = data_xy[train,], nvmax = 20)
train.fit.summary <- summary(train.fit)
#subsets_plots(train.fit.summary, 20)

# melt(train.fit.summary$rss) %>%
#     mutate(rmse = value/length(train)) %>%
#     ggplot(aes(x = seq_along(rmse), y = rmse)) + 
#     geom_line(color = "dodgerblue", alpha = 0.7) + 
#     labs(x = "Number of variables", y = "rmse") + 
#     scale_x_continuous(breaks = seq_along(1:20))

test.mat <- model.matrix(y ~., data = data_xy[test,])
val.errors = rep(NA, 20)
# calculate mse for each train model on test set
calc_mse <- function(i) {
    # extract coefficients
    coefi <- coef(train.fit, id = i)
    # multiply design matrix by coefficients to get predictions
    pred <- test.mat[, names(coefi)] %*% coefi
    # square the residuals and take square root to get MSE
    val.errors[i] <- mean((data_xy$y[test] - pred)^2)
}

test_mse <- sapply(1:20, calc_mse)
which.min(test_mse)
# melt(test_mse) %>%
#     ggplot(aes(x = seq_along(value), y = value)) + 
#     geom_line(color = "dodgerblue", alpha = 0.7) + 
#     labs(x = "Number of variables", y = "rmse") + 
#     scale_x_continuous(breaks = seq_along(1:20))


test_mse_vals <- as.tibble( melt(test_mse, value.name = "test"))  %>%
    rowid_to_column("id")
train_mse_vals <- as.tibble(melt(train.fit.summary$rss/length(train),
                                 value.name = "train")) %>%
    rowid_to_column("id")
inner_join(train_mse_vals, test_mse_vals, by = "id") %>%
    melt(id.vars = "id") %>%
    rename(data = variable, mse = value) %>%
    ggplot(aes(x = id, y = mse, color = data)) +
        geom_line()

coef(train.fit, which.min(test_mse))
betas

#for (i in paste("x_", 1:20, sep = "")) { 
#    print(unlist(map(1:20, function(x) coef(train.fit, x))[2])[i])
#}

betas <- c('(Intercept)' = 0, betas)
#coeffs_i <- sapply(1:20, function(x) coef(train.fit, x))

g <- rep(NA, 20)
for (r in 1:20) {
    sum_r <- 0
    for (j in (names(coef(train.fit, r)))) {
        #print(sum((betas[j] - coef(train.fit, r)[j])^2))
        sum_r = sum_r + ((betas[j] - coef(train.fit, r)[j])^2)
    }
    #names(sum_r) <- NULL
    g[r] <- sqrt(sum_r)
}

qplot(x = c(1:20), y = g, geom = "path")

# Exercise 11 ############################
rm(list = ls())
set.seed(1)
boston <- as.tibble(Boston)
boston
glimpse(boston)

# test, train
train <- sample(c(TRUE, FALSE), nrow(boston), replace = TRUE)
test <- !train
table(train, test)
test.mat <- model.matrix(crim ~., data = boston[test,])

# subsets
sub.all <- regsubsets(crim ~., data = boston, nvmax = (length(boston) - 1))
sub.fwd <- regsubsets(crim ~., data = boston, nvmax = (length(boston) - 1), 
                      method = "forward")
sub.bwd <- regsubsets(crim ~., data = boston, nvmax = (length(boston) - 1), 
                      method = "backward")

sub.all <- sub.fwd
sub.all <- sub.bwd
sub.all.summary <- summary(sub.all)
subsets_plots(sub.all.summary, (length(boston) - 1))
which.min(sub.all.summary$bic) 
#theoretical correction chooses full, fwd = 3, bwd = 4
rss.all <- sub.all.summary$rss[which.min(sub.all.summary$bic)]
rss.fwd <- sub.all.summary$rss[which.min(sub.all.summary$bic)]
rss.bwd <- sub.all.summary$rss[which.min(sub.all.summary$bic)]

# use train and test data
train.sub.all <- regsubsets(crim ~., data = boston[train,],
                            nvmax = length(boston))
#train.sub.all.summary <- summary(train.sub.all)
#subsets_plots(train.sub.all.summary, length(boston))
#which.min(train.sub.all.summary$bic)
train.sub.fwd <- regsubsets(crim ~., data = boston[train,],
                            nvmax = length(boston), method = "forward")
train.sub.bwd <- regsubsets(crim ~., data = boston[train,],
                            nvmax = length(boston), method = "backward")
train.sub.all <- train.sub.fwd
train.sub.all <- train.sub.bwd
val.errors = rep(NA, length(boston) - 1)
# calculate mse for each train model on test set
calc_mse <- function(i, fitted_model) {
    # extract coefficients
    coefi <- coef(fitted_model, id = i)
    # multiply design matrix by coefficients to get predictions
    pred <- test.mat[, names(coefi)] %*% coefi
    # square the residuals and take square root to get MSE
    val.errors[i] <- mean((boston$crim[test] - pred)^2)
}

test_all_mse <- sapply(1:(length(boston) - 1), calc_mse, train.sub.all)
which.min(test_all_mse) #using test set selects 2
hold_all <- test_all_mse[which.min(test_all_mse)]
hold_fwd <- test_all_mse[which.min(test_all_mse)]
hold_bwd <- test_all_mse[which.min(test_all_mse)]
qplot(x = 1:13, y = test_all_mse, geom = "path")
# full, fwd = 2, bwd = 4
# Use cross-validation

# USING CROSS-VALIDATION

k = 10 # total folds
# set.seed(1)
folds <- sample(1:k, nrow(boston), replace = TRUE)
cv.errors <- matrix(NA, k, 13, dimnames = list(NULL, paste(1:13)))

predict.regsubsets <- function(object, newdata, id, ...) {
    form <- as.formula(object$call[[2]])
    mat <- model.matrix(form, newdata)
    coefi <- coef(object, id = id)
    xvars <- names(coefi)
    mat[, xvars] %*% coefi #return vector of predictions
}

# write a function that returns the mse. also pass in the method for regsubs
# method = c("exhaustive", "backward", "forward")
# k = 1 is not working. Why?

for (j in 1:k) {
    best.fit <- regsubsets(crim ~., data = boston[folds != j,], 
                           nvmax = 13)
    for (i in 1:13) {
        pred = predict.regsubsets(best.fit, boston[folds == j,], id = i)
        cv.errors[j, i] <- mean((boston$crim[folds == j] - pred)^2)
    } # end of inside for loop
} # end of outside for loop

# average columns to get average mse across folds (k) by number of variables (i)
mean.cv.errors = apply(cv.errors, 2, mean)
plot(mean.cv.errors, type = 'b')
which.min(mean.cv.errors)
cv.full <- mean.cv.errors[which.min(mean.cv.errors)]
cv.fwd <- mean.cv.errors[which.min(mean.cv.errors)]
cv.bwd <- mean.cv.errors[which.min(mean.cv.errors)]
reg.best = regsubsets(crim ~., data = boston, nvmax = 13)

coefs_all <- coef(reg.best, which.min(mean.cv.errors))
coefs_fwd <- coef(reg.best, which.min(mean.cv.errors)) 
coefs_bwd <- coef(reg.best, which.min(mean.cv.errors))
# bwd, fwd = 12, all = 11
# ------------------ end cross validataion k = 10
# end of full subset

# Forward and backward subsets
# Ridge
# create design matrix
set.seed(1)
x = model.matrix(crim ~., boston)[,-1] #removed the column with 1s (ones)
y = boston$crim
# RIDGE alpha = 0
grid <- 10^seq(10, -2, length = 100)
cv.ridge.mod = cv.glmnet(x[train,], y[train], alpha = 0)
plot(cv.ridge.mod)
bestlam = cv.ridge.mod$lambda.min
bestlam
ridge.mod <- glmnet(x[train,], y[train], 
                    alpha = 0, lambda = grid, thresh = 1e-12)
ridge.pred <- predict(ridge.mod, s = bestlam, newx = x[test,])
mean((ridge.pred - y[test])^2)

# Least squares MSE for comparison because lambda is very small
# very slight improvement
ridge.pred <- predict(ridge.mod, s = 0, newx = x[test,])
mean((ridge.pred - y[test])^2)

# fit to full data set using lambda = bestlam
out <- glmnet(x, y, alpha = 0)
predict(out, type = "coefficients", s = bestlam)[1:14,]

### end of ridge

## LASSO
cv.lasso.mod = cv.glmnet(x[train,], y[train], alpha = 1)
plot(cv.lasso.mod)
bestlam = cv.lasso.mod$lambda.min
bestlam

lasso.mod <- glmnet(x[train,], y[train], 
                    alpha = 1, lambda = grid, thresh = 1e-12)
lasso.pred <- predict(lasso.mod, s = bestlam, newx = x[test,])
mean((lasso.pred - y[test])^2) 
# is the same as least squares but with five predictors

#full model
out <- glmnet(x, y, alpha = 1)
predict(out, type = "coefficients", s = bestlam)
plot(out)

## End of lasso

### PCR

pcr.fit <- pcr(crim ~., data = boston, 
               scale = TRUE, validation = "CV",
               subset = train)
validationplot(pcr.fit, val.type = "MSEP")

# predict using test data (M = 8, minimum MSEP)
pcr.pred <- predict(pcr.fit, x[test,], ncomp = 8)
mean((pcr.pred - y[test])^2)

# fit on full model using M = 7
pcr.fit <- pcr(y ~ x, scale = TRUE, ncomp = 8)
summary(pcr.fit)
