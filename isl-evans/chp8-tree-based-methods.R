source("load-libraries.R")

# LAB
# 8.3.1 Fittig classification trees
carseats <- as.tibble(Carseats)
glimpse(carseats)
names(carseats) <- tolower(names(carseats))
carseatsx <- carseats %>% 
    mutate(high = as.factor(if_else(sales <= 8, "No", "Yes")))

select(carseatsx, sales, high)
carseatsx %>% 
    group_by(high) %>% 
    summarise(mean.sales = mean(sales, na.rm = TRUE),
              sd.sales = sd(sales, na.rm = TRUE),
              n = n())

tree.carseatsx <- tree(high ~. -sales, carseatsx)
summary(tree.carseatsx)
plot(tree.carseatsx)
text(tree.carseatsx, pretty = 0)
tree.carseatsx

# use train, test sets
set.seed(2)
train <-  sample(1:nrow(carseatsx), 200)
carseatsx.test <-  carseatsx[-train,]
high.test <-  carseatsx$high[-train]

tree.carseatsx.train <- tree(high ~. -sales, carseatsx, subset = train)
tree.carseatsx.train
summary(tree.carseatsx.train)

# estimate test error using test set
tree.pred <- predict(tree.carseatsx.train, carseatsx.test, type = "class")
conf.mat <- table(tree.pred, high.test)
conf.mat
acc <- (conf.mat[1] + conf.mat[4])/nrow(carseatsx.test)
acc
err <- 1 - acc
err

# choosing alpha using cross validation
# corresponds to k in output
# size |T| = number of terminal nodes of tree considered in process

set.seed(3)
cv.carseatsx = cv.tree(tree.carseatsx.train,
                       FUN = prune.misclass)
names(cv.carseatsx)
cv.carseatsx

## graphs of size (terminal nodes) versus deviance, alpha vs deviance
cv.params <- tibble(size = cv.carseatsx$size, 
                    k = cv.carseatsx$k, 
                    dev = cv.carseatsx$dev)
ggp_size <- cv.params %>% 
    ggplot(aes(x = size, y = dev)) + 
    geom_line(color = "dodgerblue2") + geom_point(color = "blue")

ggp_k <- cv.params %>% 
    ggplot(aes(x = k, y = dev)) + 
    geom_line(color = "chocolate2") + geom_point(color = "chocolate4")

grid.arrange(ggp_size, ggp_k, ncol = 2, nrow = 1)
## end of graphs

## prune tree to value selected in cross validation
best.val <- cv.carseatsx$size[which.min(cv.carseatsx$dev)]
best.val
prune.carseatsx <- prune.misclass(tree.carseatsx.train, best = best.val)
# plot pruned tree
plot(prune.carseatsx)    
text(prune.carseatsx, pretty = 0)

## estimate test error using pruned tree
tree.pred <- predict(prune.carseatsx, carseatsx.test, type = "class")
conf.mat <- table(tree.pred, high.test)
conf.mat
acc <- (conf.mat[1] + conf.mat[4])/nrow(carseatsx.test)
acc
err <- 1 - acc
err


## fit to entire dataset
tree.pred.full <- predict(prune.carseatsx, carseatsx, type = "class")
conf.mat <- table(tree.pred.full, carseatsx$high)
conf.mat
acc <- (conf.mat[1] + conf.mat[4])/nrow(carseatsx)
acc

### end 

## 8.3.2 Fitting regression trees
rm(list = ls())
boston <- as.tibble(Boston)
head(boston)

set.seed(1)
train <- sample(1:nrow(boston), nrow(boston)/2)
test <- -(train)
tree.boston <- tree(medv ~., boston, subset = train)
summary(tree.boston)
# note that the RSE (unbiased) is being reported, not the MSE (biased)
# https://en.wikipedia.org/wiki/Errors_and_residuals

plot(tree.boston)
text(tree.boston, pretty = 0)

## estimate test error using test set
## see below, after cross-validation

# selecting size through cross-validation. in this case the most complex model
# is selected.
cv.boston = cv.tree(tree.boston)
plot(cv.boston$size, cv.boston$dev, type = 'b')
# the most complex tree is selected by the cross-validation

## if one wants to prune the tree
prune.boston <- prune.tree(tree.boston, best = 5)
plot(prune.boston)
text(prune.boston, pretty = 0) 
##

# using the result from cross validation - the most complex tree (= unpruned)
# this is the same as estimating test error using a validation set (see above)
yhat <- predict(tree.boston, newdata = boston[-train,])
boston.test <- boston[-train, "medv"]
plot(yhat, boston.test$medv)
abline(0, 1)
mean((yhat - boston.test$medv)^2) # biased (MSE)

# 8.3.3 bagging and random forests

set.seed(1)
bag.boston <- randomForest(medv ~., 
                           data = boston, subset = train,
                           mtry = 13, importance = TRUE)
bag.boston

## performance on test set. estimate test error on validation set
yhat.bag <- predict(bag.boston, newdata = boston[-train,])
plot(yhat.bag, boston.test$medv)
abline(0, 1)
mean((yhat.bag - boston.test$medv)^2)

# changing number of trees grown using ntree option
bag.boston <- randomForest(medv ~., 
                           data = boston, subset = train,
                           mtry = 13, importance = TRUE, ntree = 25)
bag.boston

yhat.bag <- predict(bag.boston, newdata = boston[-train,])
mean((yhat.bag - boston.test$medv)^2) # does slightly worse

## random forest with mtry = 6
set.seed(1)
rf.boston <- randomForest(medv ~., data = boston, subset = train,
                          mtry = 6, importance = TRUE)
rf.boston

# estimate test error
yhat.rf <- predict(rf.boston, newdata = boston[-train,])
mean((yhat.rf - boston.test$medv)^2)

importance(rf.boston)
varImpPlot(rf.boston)

# boosting
set.seed(1)

# Boosting grows trees sequentially. Fitting is done on the residuals not on 
# the response. It learns based on the learning rate specified by lambda.
# Smaller lambdas = slower learning which requires bigger trees.
# Interaction depth: A depth of one is a stump.

boost.boston <- gbm(medv ~., data = boston[train,], distribution = "gaussian",
                    n.trees = 5000, interaction.depth = 4)
summary(boost.boston)
par(mfrow = c(1, 2))
plot(boost.boston, i = "rm")
plot(boost.boston, i = "lstat")
plot(boost.boston, i = "dis")
plot(boost.boston, i = "rad")

## Fit on test data. Estimate train error
yhat.boost <- predict(boost.boston, newdata = boston[-train,], n.trees = 5000)
mean((yhat.boost - boston.test$medv)^2)

# repeat with different value of shrinkage parameter
boost.boston <- gbm(medv ~., data = boston[train,], distribution = "gaussian",
                    n.trees = 5000, interaction.depth = 4,
                    shrinkage = 0.2, verbose = FALSE)
yhat.boost <- predict(boost.boston, newdata = boston[-train,], n.trees = 5000)
mean((yhat.boost - boston.test$medv)^2)

## EXERCISES ------------------------------
# 7
set.seed(1)
rf.mphalf <- function(num_trees, mtries) {
    rf.boston.mphalf <- randomForest(medv ~., data = boston, subset = train,
                              mtry = mtries, importance = TRUE, ntree = num_trees)
    yhat.rf <- predict(rf.boston.mphalf, newdata = boston[-train,])
    mean((yhat.rf - boston.test$medv)^2)
}

# https://stackoverflow.com/questions/6827299/r-apply-function-with-multiple-parameters
#sapply(1:10, rf.mphalf, mtries = 6)

# inside apply controls number of trees
# outside controls number of predictors 'm' being considered at each split
num_trees <- seq(5, 500, 5)
mse_vals <- sapply(c(4, 7, 10, 13), 
                   function(m) sapply(num_trees, rf.mphalf, mtries = m))

# https://stackoverflow.com/questions/16384933/create-an-id-row-number-column
mse_values <- as_tibble(mse_vals)
names(mse_values) <- c("m4", "m7", "m10", "m13", "id")
mse_values <- mutate(mse_values, id = row_number() * 5)

mse_values %>% 
    ggplot(aes(x = id)) + 
        geom_line(aes(y = m4), color = "dodgerblue2") + 
        geom_line(aes(y = m7), color = "chocolate4") +
        geom_line(aes(y = m10), color = "forestgreen") +
        geom_line(aes(y = m13), color = "grey0") +
        labs(y = "m", x = "Number of Trees") 

# https://stackoverflow.com/questions/10349206/add-legend-to-ggplot2-line-plot

melt(mse_values, id = "id", value.name = "error", variable.name = "m") %>%
    ggplot() + 
        geom_line(aes(x = id, y = error, colour = m)) +
        labs(y = "Test Classification Error", x = "Number of Trees") +
        scale_colour_manual(values = c("dodgerblue2", 
                                       "chocolate4", 
                                       "forestgreen",
                                       "grey0"))

# can use the values generated by the built-in function
# convert to matrix: tibble gives an error
boston.mat <- as.matrix(boston) # tibble gives error 

x.mat.train <- boston.mat[train, -14]
x.mat.test <- boston.mat[-train, -14]

y.mat.train <- boston.mat[train, 14]
y.mat.test <- boston.mat[-train, 14]

rf.mse.ntree <- function(num_trees, mtries) {
    rf.boston.m13 <- randomForest(x = x.mat.train, y = y.mat.train,
                                     xtest = x.mat.test, ytest = y.mat.test,
                                     ntree = 500, mtry = mtries,
                                     importance = TRUE)
    
    # rf.boston.m13$test$mse: mse for each B (tree size)
    rf.boston.m13$test$mse[num_trees]
} # end of function rf.mse.ntree

num_trees <- seq(1, 500)
set.seed(1)
mse_vals <- sapply(c(4, 7, 10, 13), 
                   function(m) sapply(num_trees, rf.mse.ntree, mtries = m))
mse_vals

melt(mse_values, id = "id", value.name = "error", variable.name = "m") %>%
    ggplot() + 
    geom_line(aes(x = id, y = error, colour = m)) +
    labs(y = "Test Classification Error", x = "Number of Trees") +
    scale_colour_manual(values = c("dodgerblue2", 
                                   "chocolate4", 
                                   "forestgreen",
                                   "grey0"))

## Exercise 8 ------------
rm(list = ls())
carseats <- as_tibble(Carseats)
glimpse(carseats)
names(carseats) <- tolower(names(carseats))

set.seed(1)
train <- sample(c(TRUE, FALSE), nrow(carseats), replace = TRUE)
test <- !train
table(train, test)

# fit regression tree to training set
tree.train <- tree(sales ~., data = carseats[train,])
summary(tree.train)
plot(tree.train)
text(tree.train, pretty = 0)

# use the model to generate predictions on test set
# calculate mse
yhat <- predict(tree.train, newdata = carseats[test,])
carseats.test <- carseats$sales[test]
plot(yhat, carseats.test)
abline(0, 1, col = "red")
mean((yhat - carseats.test)^2)

# get size using cross validation
cv.carseats <- cv.tree(tree.train)
qplot(x = cv.carseats$size, y = cv.carseats$dev, geom = "line")
best_size <- cv.carseats$size[which.min(cv.carseats$dev)] # selected tree size 
best_size

# prune the tree at size = 4
prune.carseats <- prune.tree(tree.train, best = best_size)
plot(prune.carseats)
text(prune.carseats)

# use the pruned tree
yhat <- predict(prune.carseats, newdata = carseats[test,])
carseats.test <- carseats$sales[test]
plot(yhat, carseats.test)
abline(0, 1)
mean((yhat - carseats.test)^2)

# USE Bagging
# set.seed(1)
bag.carseats <- randomForest(sales ~. , data = carseats, subset = train,
                             mtry = 10, importance = TRUE)
bag.carseats

yhat.bag <- predict(bag.carseats, newdata = carseats[test,])
plot(yhat.bag, carseats$sales[test])
abline(0, 1, col = "dodgerblue")
bag.carseats.mse <- mean((yhat.bag - carseats$sales[test])^2)
bag.carseats.mse

importance(bag.carseats)
varImpPlot(bag.carseats)

# On full data
bag.carseats <- randomForest(sales ~. , data = carseats,
                             mtry = 10, importance = TRUE)
bag.carseats

yhat.bag <- predict(bag.carseats)
plot(yhat.bag, carseats$sales[test])
abline(0, 1, col = "dodgerblue")
bag.carseats.mse <- mean((yhat.bag - carseats$sales)^2)
bag.carseats.mse

importance(bag.carseats)
varImpPlot(bag.carseats)


## random forests
# set.seed(1)
rf <- function(mtries) {
    rf.carseats <- randomForest(sales ~. , data = carseats, subset = train,
                                 mtry = mtries, importance = TRUE)
    rf.carseats
    
#    importance(rf.carseats)
#    varImpPlot(rf.carseats)
    
    yhat.bag <- predict(rf.carseats, newdata = carseats[test,])
#    plot(yhat.bag, carseats$sales[test])
#    abline(0, 1, col = "chocolate4")
    rf.carseats.mse <- mean((yhat.bag - carseats$sales[test])^2)
    rf.carseats.mse
}

rf.mse.mtry <- sapply(2:10, rf)
rf.mse.mtry[which.min(rf.mse.mtry)]
best_mtry <- which.min(rf.mse.mtry) + 1 #search started at 2

## Fit to full dataset
rf.carseats <- randomForest(sales ~. , data = carseats,
                            mtry = best_mtry, importance = TRUE)
rf.carseats

importance(rf.carseats)
varImpPlot(rf.carseats)

yhat.bag <- predict(rf.carseats)
#    plot(yhat.bag, carseats$sales[test])
#    abline(0, 1, col = "chocolate4")
rf.carseats.mse <- mean((yhat.bag - carseats$sales)^2)
rf.carseats.mse # mse from full data


## Exercise 9
rm(list = ls())
set.seed(1)
oj <- as_tibble(OJ)
oj
glimpse(oj)
names(oj) <- tolower(names(oj))
dim(oj)

train <- sample(nrow(oj), 800)
test <- -(train)

## fit classification tree to training set
oj.train <- tree(purchase ~., data = oj, subset = train)
summary(oj.train)
plot(oj.train)
text(oj.train, pretty = 0)

# look at output or calculate training error
train.pred <- predict(oj.train, type = "class")
conf.mat <- table(train.pred, oj$purchase[train])
conf.mat
acc <- (conf.mat[1] + conf.mat[4])/length(oj$purchase[train])
acc
err <- 1 - acc
err

## fit tree to test data and record predictions
test.pred <- predict(oj.train, newdata = oj[test,], type = "class")
conf.mat <- table(test.pred, oj$purchase[test])
conf.mat
acc <- (conf.mat[1] + conf.mat[4])/length(oj$purchase[test])
acc
err <- 1 - acc
err # Test error rate (Unpruned tree)


## use cross validation to find optimal tree size
cv.oj.train <- cv.tree(oj.train, FUN = prune.misclass)
names(cv.oj.train)
cv.oj.train

cv.params <- tibble(size = cv.oj.train$size, 
                    k = cv.oj.train$k, 
                    dev = cv.oj.train$dev)
ggp_size <- cv.params %>% 
    ggplot(aes(x = size, y = dev)) + 
    geom_line(color = "dodgerblue2") + geom_point(color = "blue")

ggp_k <- cv.params %>% 
    ggplot(aes(x = k, y = dev)) + 
    geom_line(color = "chocolate2") + geom_point(color = "chocolate4")
grid.arrange(ggp_size, ggp_k, nrow = 1, ncol = 2)

## prune tree (size = 2)

prune.oj <- prune.misclass(oj.train, best = 2)
summary(prune.oj)
plot(prune.oj)
text(prune.oj, pretty = 0)


## fit pruned tree on test set
tree.pred <- predict(prune.oj, newdata = oj[test,], type = "class")
conf.mat <- table(tree.pred, oj$purchase[test])
conf.mat
acc <- (conf.mat[1] + conf.mat[4])/length(oj$purchase[test])
acc
err <- 1 - acc
err

## Exercise 9
## boosting
dev.off()
rm(list = ls())

hitters <- as_tibble(Hitters)
glimpse(hitters)
names(hitters) <- tolower(names(Hitters))

# https://stackoverflow.com/questions/26665319/removing-na-in-dplyr-pipe
hittersx <- hitters %>%
    filter(!is.na(salary)) %>%
    mutate(log.salary = log(salary))
head(hittersx)

set.seed(1)
train <- sample(nrow(hittersx), 200)
test <- -(train)

boost.hittersx <- gbm(log.salary ~. -salary, data = hittersx[train, ], 
                     distribution = "gaussian", n.trees = 1000, 
                     interaction.depth = 4)

# https://stackoverflow.com/questions/10941225/horizontal-barplot-in-ggplot2
# summary(boost.hittersx) %>% 
#     as_tibble() %>%
#     ggplot(aes(y = rel.inf, x = var)) + geom_col() +
#     coord_flip()

summary(boost.hittersx) %>% 
    ggplot(aes(x = reorder(var, rel.inf), y = rel.inf)) +
    geom_bar(stat = 'identity', fill = "dodgerblue2") +
    labs(x = "Predictors", y = "Relative Influence") +
    coord_flip()

## partial dependence plots. effect of a variable after integrating out the rest
par(mfrow = c(1, 2))
plot(boost.hittersx, i = "catbat")
plot(boost.hittersx, i = "crbi")
plot(boost.hittersx, i = "chits")
plot(boost.hittersx, i = "putouts")
plot(boost.hittersx, i = "walks")
plot(boost.hittersx, i = "hits")

## predict on test data
yhat.boost <- predict(boost.hittersx, newdata = hittersx[test,], n.trees = 1000)
mean((yhat.boost - hittersx$log.salary[test])^2) # biased MSE

## change shrinkage parameter and calculate test mse

boost.fit <- function(shrink, holdout) {
    boost.hittersx <- gbm(log.salary ~. -salary, data = hittersx[train, ], 
                          distribution = "gaussian", n.trees = 1000, 
                          interaction.depth = 4, shrinkage = shrink, verbose = F)
    # predictions on test data
    yhat.boost <- predict(boost.hittersx, newdata = hittersx[test,], n.trees = 1000)
    mean((yhat.boost - hittersx$log.salary[test])^2)
}

shrinkage <-  10^seq(-10, -1, by = 0.1)
test.errors <- sapply(shrinkage, boost.fit)
as_tibble(cbind(shrinkage, test.errors)) %>% 
    ggplot() + 
    geom_line(aes(x = shrinkage, y = test.errors))

# function fits boosting model on train, test sets depending on the value of 
# holdout. train = FALSE, test = TRUE
boost.fit <- function(shrink, holdout) {
    boost.hittersx <- gbm(log.salary ~. -salary, data = hittersx[train, ], 
                          distribution = "gaussian", n.trees = 1000, 
                          interaction.depth = 4, shrinkage = shrink, 
                          verbose = F)
    if (!holdout) {
        yhat.boost <- predict(boost.hittersx, newdata = hittersx[train,], n.trees = 1000)
        mean((yhat.boost - hittersx$log.salary[train])^2)
    } # end of IF
    else {
        yhat.boost <- predict(boost.hittersx, newdata = hittersx[test,], n.trees = 1000)
        mean((yhat.boost - hittersx$log.salary[test])^2)
    } # end of else
} # end of function

train.errors <- sapply(shrinkage, boost.fit, holdout = FALSE)
test.errors <- sapply(shrinkage, boost.fit, holdout = TRUE)

par(mfrow = c(1, 2))
hist(train.errors) 
hist(test.errors)

errors.table <- as_tibble(cbind(shrinkage, train.errors, test.errors))
errors.table.long <- melt(errors.table, id = "shrinkage", 
                          variable.name = "error.type", 
                          value.name = "mse")

which.min(errors.table$test.errors)
which.min(errors.table$train.errors)
errors.table.long %>%
        ggplot() +
        geom_line(aes(x = shrinkage, y = mse, colour = error.type)) +
        geom_vline(xintercept = 0.0038)


errors.table.long %>%
    ggplot(aes(x = error.type, y = mse, color = cut_number(shrinkage, 4))) + 
    geom_violin() + geom_jitter()

errors.table.long %>% 
    group_by(cut_number(shrinkage, 4)) %>% 
    summarise(mean(mse))

errors.table.long %>% 
    group_by(error.type, cut_number(shrinkage, 4)) %>% 
    summarise(mean(mse))

errors.table.long %>% 
    group_by(cut_number(shrinkage, 4), error.type) %>% 
    summarise(mean(mse))

## bagging
bag.hittersx <- randomForest(log.salary ~. -salary, data = hittersx, 
                             subset = train,
                             mtry = 19, importance = TRUE)
bag.hittersx

importance(bag.hittersx)
varImpPlot(bag.hittersx)

yhat.bag <- predict(bag.hittersx, newdata = hittersx[test,])
plot(yhat.bag, hittersx$log.salary[test])
abline(0, 1, col = "dodgerblue")
bag.hittersx.mse <- mean((yhat.bag - hittersx$log.salary[test])^2)
bag.hittersx.mse

############
# Example 11

rm(list = ls())
caravan <- as_tibble(Caravan)
names(caravan) <- tolower(names(caravan))
dim(caravan)

caravanx <- caravan %>% 
    mutate(purchasex = if_else(purchase == "Yes", 1, 0)) 
with(caravanx, table(purchase, purchasex))

set.seed(1)
train <- sample(nrow(caravanx), 1000)
test <- -(train)
dim(caravanx[train,])
dim(caravanx[test,])

# boosting
boost.caravanx <- gbm(purchasex ~. -purchase , data = caravanx[train, ], 
                      distribution = "bernoulli", n.trees = 1000, 
                      interaction.depth = 4, shrinkage = 0.01)
boost.caravanx
summary(boost.caravanx) %>% 
    filter(rel.inf > 0) %>% 
    ggplot(aes(x = reorder(var, rel.inf), y = rel.inf)) +
    geom_bar(stat = 'identity', fill = "dodgerblue2") +
    labs(x = "Predictors", y = "Relative Influence (> zero)") +
    coord_flip()

yhat.boost <- predict(boost.caravanx, newdata = caravanx[test,], 
                      n.trees = 1000, type = "response")
class.preds <- rep(0, length(yhat.boost))
class.preds[yhat.boost > 0.2] <- 1
conf.mat <- table(class.preds, caravanx$purchasex[test])
conf.mat
acc <- (conf.mat[1] + conf.mat[4])/length(caravanx$purchasex[test])
acc
err <- 1 - acc
err