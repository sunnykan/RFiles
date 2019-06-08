source("load-libraries.R")

defaults <- as.tibble(Default)
glimpse(defaults)
table(defaults$default)
table(defaults$student)
table(defaults$default, defaults$student)

ggplot(data = defaults, aes(x = balance, y = income, color = default)) +
    geom_point(alpha = 0.2)

ggp1 <- ggplot(data = defaults, aes(x = default, y = balance))
ggp1 + geom_boxplot(fill = "lightblue", 
                   colour = "#3366FF", 
                   outlier.colour = "red", 
                   outlier.shape = 1)
ggp1 + geom_boxplot(aes(colour = student))

ggp2 <- ggplot(data = defaults, aes(x = default, y = income))
ggp2 + geom_boxplot(fill = "lightblue", 
                    colour = "#3366FF", 
                    outlier.colour = "red", 
                    outlier.shape = 1)
ggp2 + geom_boxplot(aes(colour = student))

# LAB
# Stock market

stockmkt <- as.tibble(Smarket)
names(stockmkt) <- tolower(names(stockmkt))
glimpse(stockmkt)
stockmkt
summary(stockmkt)
#pairs(stockmkt)
#ggpairs(stockmkt, aes(colour = direction, alpha = 0.2))

cor(stockmkt[,-9])
glm.fits <- glm(direction ~ lag1 + lag2 + lag3 + lag4 + lag5 + volume, 
                data = stockmkt, 
                family = binomial)
tidy(glm.fits)
glance(glm.fits)
glm.probs <- predict(glm.fits, type = "response")
round(glm.probs[1:10], 3)

glm.pred <- rep("Down", 1250)
glm.pred[glm.probs > 0.5] = "Up"
#confusion matrix
table(glm.pred, stockmkt$direction)

#create training set
train <- (stockmkt$year < 2005)
stockmkt.2005 <- stockmkt[!train,]
dim(stockmkt.2005)
direction.2005 <- stockmkt$direction[!train]

#fit to training data
glm.fits.train <- glm(direction ~ lag1 + lag2 + lag3 + lag4 + lag5 + volume, 
                data = stockmkt, 
                family = binomial,
                subset = train)
#predict using test data
glm.probs.test <- predict(glm.fits.train, stockmkt.2005, type = "response")
glm.pred.test = rep("Down", 252)
glm.pred.test[glm.probs.test > 0.5] = "Up"
table(glm.pred.test, direction.2005)
mean(glm.pred.test == direction.2005) #test accuracy rate
mean(glm.pred.test != direction.2005) #test error rate

#fit to less complex model
glm.fits.train2 <- glm(direction ~ lag1 + lag2, 
                      data = stockmkt, 
                      family = binomial,
                      subset = train)
#predict using test data
glm.probs.test2 <- predict(glm.fits.train2, stockmkt.2005, type = "response")
glm.pred.test2 = rep("Down", 252)
glm.pred.test2[glm.probs.test2 > 0.5] = "Up"
table(glm.pred.test2, direction.2005)
mean(glm.pred.test2 == direction.2005)
mean(glm.pred.test2 != direction.2005)

#predict at particular values
predict(glm.fits.train2, 
        newdata = data.frame(lag1 = c(1.2, 1.5), lag2 = c(1.1, -0.8)),
        type = "response")

#Fit LDA
lda.fit <- lda(direction ~ lag1 + lag2, data = stockmkt, subset = train)
lda.fit
#predict using test data
lda.pred <- predict(lda.fit, stockmkt.2005)
names(lda.pred)
lda.class <- lda.pred$class
table(lda.class, direction.2005)
mean(lda.class == direction.2005)
mean(lda.class != direction.2005)

sum(lda.pred$posterior[,1] >= 0.5)
sum(lda.pred$posterior[,1] < 0.5)

#Fit QDA
qda.fit <- qda(direction ~ lag1 + lag2, data = stockmkt, subset = train)
qda.fit
qda.class <- predict(qda.fit, stockmkt.2005)$class
table(qda.class, direction.2005)
mean(qda.class == direction.2005)
mean(qda.class != direction.2005)

#Fit KNN
train.X = cbind(stockmkt$lag1, stockmkt$lag2)[train,] #training set
test.X = cbind(stockmkt$lag1, stockmkt$lag2)[!train,] #test set
train.direction = stockmkt$direction[train] #class labels

set.seed(1)
knn.pred = knn(train.X, test.X, train.direction, k = 1)
table(knn.pred, direction.2005)
mean(knn.pred == direction.2005)

knn.pred = knn(train.X, test.X, train.direction, k = 3)
table(knn.pred, direction.2005)
mean(knn.pred == direction.2005)

knn.pred = knn(train.X, test.X, train.direction, k = 5)
table(knn.pred, direction.2005)
mean(knn.pred == direction.2005)

#KNN on caravan data
caravan <- as.tibble(Caravan)
glimpse(caravan)
dim(caravan)
names(caravan) <- tolower(names(caravan))
summary(caravan$purchase)

standardized.X <-  scale(caravan[,-86])
var(standardized.X[,1])
var(standardized.X[,2])

set.seed(1)
test <-  sample(1:nrow(caravan), 4822) #for comparison with boosting in Chp 8
# test <- 1:1000
train.X <-  standardized.X[-test,]
test.X <-  standardized.X[test,]
train.Y <-  caravan$purchase[-test]
test.Y <-  caravan$purchase[test]

#set.seed(1)
knn.pred <- knn(train.X, test.X, train.Y, k = 1)
table(knn.pred, test.Y)
mean(knn.pred != test.Y)
mean(test.Y != "No")

knn.pred <- knn(train.X, test.X, train.Y, k = 3)
table(knn.pred, test.Y)
mean(knn.pred != test.Y)
mean(test.Y != "No")

knn.pred <- knn(train.X, test.X, train.Y, k = 5)
table(knn.pred, test.Y)
mean(knn.pred != test.Y)
mean(test.Y != "No")

#Use Logistic
glm.fits <- glm(purchase ~., data = caravan, family = binomial, subset = -test)
glm.probs <- predict(glm.fits, caravan[test,], type = "response")
#glm.pred <- rep("No", 1000)
glm.pred <- rep("No", 4822) # Boosting chapter 8
glm.pred[glm.probs > 0.5] = "Yes"
table(glm.pred, test.Y)

#lower cut-off
#glm.pred <- rep("No", 1000)
glm.pred <- rep("No", 4822) # Boosting chapter 8
glm.pred[glm.probs > 0.20] = "Yes"
table(glm.pred, test.Y)

# --------------------------------
#Exercise 10
weekly <- as_tibble(Weekly)
glimpse(weekly)
summary(weekly)
names(weekly) <- tolower(names(weekly))
names(weekly)
ggpairs(weekly, aes(colour = direction, alpha = 0.2))
ggplot(data = weekly, aes(x = lag1, y = today, color = direction)) +
    geom_point()
cor(weekly[,-9])

glm.fit.full <- glm(direction ~ lag1 + lag2 + lag3 + lag4 + lag5 + volume,
                    data = weekly,
                    family = binomial)
tidy(glm.fit.full)
glance(glm.fit.full)
glm.probs <- predict(glm.fit.full, type = "response")
round(head(glm.probs), 3)
round(tail(glm.probs), 3)
summary(glm.probs)
glm.pred <- rep("Down", 1089)
glm.pred[glm.probs > 0.5] <- "Up" 
table(glm.pred, weekly$direction)
#accuracy
mean(glm.pred == weekly$direction)
(54 + 557)/1089
#overall error (1 - accuracy)
mean(glm.pred != weekly$direction)
(430 + 48)/1089
#error conditional on being down
430/(430 + 54)
#error conditional on being up
48/(48 + 557)
#model predicts as up: of these how many are mistakes?
430/(430 + 557)
#baseline
table(weekly$direction)
484/1089

#Using lag2
train <- weekly$year < 2009
weekly.0910 <- weekly[!train,]
direction.0910 <- weekly$direction[!train]

glm.fits.train <- glm(direction ~ lag2,
                      data = weekly,
                      family = binomial,
                      subset = train)
glm.fits.probs <- predict(glm.fits.train, weekly.0910, type = "response")
glm.pred <- rep("Down", 104)
glm.pred[glm.fits.probs > 0.5] <- "Up"
table(glm.pred, direction.0910)
mean(glm.pred == direction.0910)
mean(glm.pred != direction.0910)
# number of mistakes when prediction says up
34/(34 + 56)

# Repeat using LDA
lda.fit <- lda(direction ~ lag2,
               data = weekly,
               subset = train)
lda.pred <- predict(lda.fit, weekly.0910)
lda.class <- lda.pred$class
table(lda.class, direction.0910)
mean(lda.class == direction.0910)
mean(lda.class != direction.0910)

# Repeat using QDA
qda.fit <- qda(direction ~ lag2,
               data = weekly,
               subset = train)
qda.pred <- predict(qda.fit, weekly.0910)
qda.class <- qda.pred$class
table(qda.class, direction.0910)
mean(qda.class == direction.0910)
mean(qda.class != direction.0910)

# KNN
train.X = (weekly$lag1)[train]
# convert to matrix
dim(train.X) <-  c(985, 1)
# convert to matrix
test.X = (weekly$lag1)[!train]
dim(test.X) = c(104, 1)
train.direction = weekly$direction[train] #class labels

dim(train.X)
dim(test.X)
length(train.direction)

set.seed(1)
knn.pred = knn(train.X, test.X, train.direction, k = 1)
table(knn.pred, direction.0910)
mean(knn.pred == direction.0910)
mean(knn.pred != direction.0910)

knn.pred = knn(train.X, test.X, train.direction, k = 3)
table(knn.pred, direction.0910)
mean(knn.pred == direction.0910)
mean(knn.pred != direction.0910)

knn.pred = knn(train.X, test.X, train.direction, k = 5)
table(knn.pred, direction.0910)
mean(knn.pred == direction.0910)
mean(knn.pred != direction.0910)


#Using lag1, lag2
train <- weekly$year < 2009
weekly.0910 <- weekly[!train,]
direction.0910 <- weekly$direction[!train]

glm.fits.train <- glm(direction ~ lag1 + lag2,
                      data = weekly,
                      family = binomial,
                      subset = train)
glm.fits.probs <- predict(glm.fits.train, weekly.0910, type = "response")
glm.pred <- rep("Down", 104)
glm.pred[glm.fits.probs > 0.5] <- "Up"
table(glm.pred, direction.0910)
mean(glm.pred == direction.0910)
mean(glm.pred != direction.0910)
# number of mistakes when prediction says up
34/(34 + 56)

#Using poly lag2
glm.fits.train <- glm(direction ~ poly(lag2, 2),
                      data = weekly,
                      family = binomial,
                      subset = train)
tidy(glm.fits.train)
glm.fits.probs <- predict(glm.fits.train, weekly.0910, type = "response")
glm.pred <- rep("Down", 104)
glm.pred[glm.fits.probs > 0.5] <- "Up"
table(glm.pred, direction.0910)
mean(glm.pred == direction.0910)
mean(glm.pred != direction.0910)

# Repeat using QDA
qda.fit <- qda(direction ~ poly(lag2, 2),
               data = weekly,
               subset = train)
qda.pred <- predict(qda.fit, weekly.0910)
qda.class <- qda.pred$class
table(qda.class, direction.0910)
mean(qda.class == direction.0910)
mean(qda.class != direction.0910)

# EXERCISE 11 Auto data
rm(list = ls())
auto <- as.tibble(Auto)
glimpse(auto)

auto <- mutate(auto, mpg01 = as.factor(if_else(auto$mpg >= median(auto$mpg), 1, 0)))
auto <- mutate(auto, origin = as.factor(origin))
#auto <- mutate(auto, origin = as.factor(cylinders))
glimpse(auto)
auto$cylinders <- as.factor(auto$cylinders)
table(auto$mpg01)

dev.off()
ggpairs(auto[,-9])

# displacement
dev.off()
ggp1 <- ggplot(auto, mapping = aes(x = mpg01, y = displacement))
ggp1 + geom_boxplot(fill = "lightblue", 
                    colour = "#3366FF", 
                    outlier.colour = "red", 
                    outlier.shape = 1)
ggp1 + geom_boxplot(aes(colour = origin))
ggp1 + geom_boxplot(aes(colour = cylinders))

                    
# weight
ggp2 <- ggplot(auto, mapping = aes(x = mpg01, y = weight))
ggp2 + geom_boxplot(fill = "lightblue", 
                    colour = "#3366FF", 
                    outlier.colour = "red", 
                    outlier.shape = 1)
ggp2 + geom_boxplot(aes(colour = origin))
ggp2 + geom_boxplot(aes(colour = cylinders))

# horsepower
ggp3 <- ggplot(auto, mapping = aes(x = mpg01, y = horsepower))
ggp3 + geom_boxplot(fill = "lightblue", 
                    colour = "#3366FF", 
                    outlier.colour = "red", 
                    outlier.shape = 1)
ggp3 + geom_boxplot(aes(colour = origin))
ggp3 + geom_boxplot(aes(colour = cylinders))

# acceleration
ggp4 <- ggplot(auto, mapping = aes(x = mpg01, y = acceleration))
ggp4 + geom_boxplot(fill = "lightblue", 
                    colour = "#3366FF", 
                    outlier.colour = "red", 
                    outlier.shape = 1)
ggp4 + geom_boxplot(aes(colour = origin))
ggp4 + geom_boxplot(aes(colour = cylinders))

#logistic
glm.fit <- glm(mpg01 ~ acceleration + 
                   displacement + 
                   origin + 
                   displacement:origin, 
               data = auto,
               family = binomial)

tidy(glm.fit)
glance(glm.fit)
glm.probs <- predict(glm.fit, type = "response")
glm.pred <- rep(0, 392)
glm.pred[glm.probs >= 0.5] <- 1
table(glm.pred, auto$mpg01)
#accuracy
mean(glm.pred == auto$mpg01)
(54 + 557)/1089
#overall error (1 - accuracy)
mean(glm.pred != auto$mpg01)

#displacement only
glm.fit <- glm(mpg01 ~ displacement,
               data = auto,
               family = binomial)

tidy(glm.fit)
glance(glm.fit)
glm.probs <- predict(glm.fit, type = "response")
glm.pred <- rep(0, 392)
glm.pred[glm.probs >= 0.5] <- 1
table(glm.pred, auto$mpg01)
#accuracy
mean(glm.pred == auto$mpg01)
#overall error (1 - accuracy)
mean(glm.pred != auto$mpg01)

#displacement + origin + interaction
glm.fit <- glm(mpg01 ~ displacement + 
                   origin + 
                   displacement:origin, 
               data = auto,
               family = binomial)

tidy(glm.fit)
glance(glm.fit)
glm.probs <- predict(glm.fit, type = "response")
glm.pred <- rep(0, 392)
glm.pred[glm.probs >= 0.5] <- 1
table(glm.pred, auto$mpg01)
#accuracy
mean(glm.pred == auto$mpg01)
#overall error (1 - accuracy)
mean(glm.pred != auto$mpg01)

#displacement poly
glm.fit <- glm(mpg01 ~ poly(displacement, 3),
               data = auto,
               family = binomial)

tidy(glm.fit)
glance(glm.fit)
glm.probs <- predict(glm.fit, type = "response")
glm.pred <- rep(0, 392)
glm.pred[glm.probs >= 0.5] <- 1
table(glm.pred, auto$mpg01)
#accuracy
mean(glm.pred == auto$mpg01)
#overall error (1 - accuracy)
mean(glm.pred != auto$mpg01)

#weight only
glm.fit <- glm(mpg01 ~ weight,
               data = auto,
               family = binomial)

tidy(glm.fit)
glance(glm.fit)
glm.probs <- predict(glm.fit, type = "response")
glm.pred <- rep(0, 392)
glm.pred[glm.probs >= 0.5] <- 1
table(glm.pred, auto$mpg01)
#accuracy
mean(glm.pred == auto$mpg01)
#overall error (1 - accuracy)
mean(glm.pred != auto$mpg01)

# poly weight 
glm.fit <- glm(mpg01 ~ poly(weight, 3),
               data = auto,
               family = binomial)

tidy(glm.fit)
glance(glm.fit)
glm.probs <- predict(glm.fit, type = "response")
glm.pred <- rep(0, 392)
glm.pred[glm.probs >= 0.5] <- 1
table(glm.pred, auto$mpg01)
#accuracy
mean(glm.pred == auto$mpg01)
#overall error (1 - accuracy)
mean(glm.pred != auto$mpg01)

### USING test set
## LDA
set.seed(1)
test_idx <- sample(1:nrow(auto),39,replace = FALSE)
train <- auto[-test_idx,]
test <- auto[test_idx,]
test.labels <- test$mpg01

lda.fit <- lda(mpg01 ~ displacement + origin + displacement:origin,
               data = train)
lda.pred <- predict(lda.fit, test)
lda.class <- lda.pred$class
table(lda.class, test.labels)
mean(lda.class == test.labels)
mean(lda.class != test.labels)

## QDA
qda.fit <- qda(mpg01 ~ displacement + origin + displacement:origin,
               data = train)
qda.pred <- predict(qda.fit, test)
qda.class <- qda.pred$class
table(qda.class, test.labels)
mean(qda.class == test.labels)
mean(qda.class != test.labels)

# LOGISTIC: displacement + origin + interaction
glm.fit <- glm(mpg01 ~ displacement + 
                   origin + 
                   displacement:origin, 
               data = train,
               family = binomial)

tidy(glm.fit)
glance(glm.fit)
glm.probs <- predict(glm.fit, test, type = "response")
glm.pred <- rep(0, 39)
glm.pred[glm.probs >= 0.5] <- 1
table(glm.pred, test.labels)
#accuracy
mean(glm.pred == test.labels)
#overall error (1 - accuracy)
mean(glm.pred != test.labels)

# KNN
standardized.X <-  scale(auto[, c(3, 6)])
var(standardized.X[,1])
var(standardized.X[,2])

train.X <-  standardized.X[-test_idx,]
test.X <-  standardized.X[test_idx,]
train.Y <-  auto$mpg01[-test_idx]
test.Y <-  auto$mpg01[test_idx]

set.seed(1)
knn.pred <- knn(train.X, test.X, train.Y, k = 1)
table(knn.pred, test.Y)
mean(knn.pred != test.Y)
mean(knn.pred == test.Y)

knn.pred <- knn(train.X, test.X, train.Y, k = 3)
table(knn.pred, test.Y)
mean(knn.pred != test.Y)
mean(knn.pred == test.Y)

knn.pred <- knn(train.X, test.X, train.Y, k = 5)
table(knn.pred, test.Y)
mean(knn.pred != test.Y)
mean(knn.pred == test.Y)

knn.pred <- knn(train.X, test.X, train.Y, k = 7)
table(knn.pred, test.Y)
mean(knn.pred != test.Y)
mean(knn.pred == test.Y)

# EXERCISE 12
rm(list = ls())
power1 <- function() {2^3}
power1()

power2 <- function(x, a) {x^a}
power2(2, 3)
power2(2, 10)
power2(10, 3)

power3 <- function(x, a) {
    val = x^a
    return(val)
    }

power3(2, 4)
typeof(power3(2, 4))

#http://www.sthda.com/english/wiki/ggplot2-axis-scales-and-transformations#log-and-sqrt-transformations
x <- 1:10
y <- power3(x, 2)
plot(x, y)

# Exercise 13
rm(list = ls())
boston <- as.tibble(Boston)
boston
boston <- mutate(boston, crim_med = if_else(crim >= median(crim), 1, 0))
table(boston$crim_med)
glimpse(boston)
#ggpairs(boston)

set.seed(1)
test_idx <- sample(1:nrow(boston), 100, replace = FALSE) 
train <- boston[-test_idx,]
test <- boston[test_idx,]
test.labels <- test$crim_med


performance <- function(fitted_model) {
    glm.probs <- predict(fitted_model, test, type = "response")
    glm.pred <- rep(0, length(glm.probs))
    glm.pred[glm.probs >= 0.5] <- 1
    table(glm.pred)
    print(table(glm.pred, test.labels))
    mean(glm.pred == test.labels)
    mean(glm.pred != test.labels)
}

# LOGISTIC
glm.fit <- glm(crim_med ~ indus + nox + age + dis + 
                   rad + tax + black + lstat + medv,
               data = train,
               family = binomial)
tidy(glm.fit)
performance(glm.fit)

glm.fit <- glm(crim_med ~ indus + nox + age + dis + 
                   rad + tax + black + lstat,
               data = train,
               family = binomial)
tidy(glm.fit)
performance(glm.fit)

glm.fit <- glm(crim_med ~ nox + age + dis + 
                   rad + tax,
               data = train,
               family = binomial)
tidy(glm.fit)
performance(glm.fit)

glm.fit <- glm(crim_med ~ nox + age + dis + rad + tax + black + medv,
               data = train,
               family = binomial)
tidy(glm.fit)
performance(glm.fit)

glm.fit <- glm(crim_med ~ nox + rad + tax,
               data = train,
               family = binomial)
tidy(glm.fit)
performance(glm.fit)

glm.fit <- glm(crim_med ~ nox + poly(rad, 2) + poly(tax, 2),
               data = train,
               family = binomial)
tidy(glm.fit)
performance(glm.fit)

#LDA
lda.fit <- lda(crim_med ~ nox + rad + tax, data = train)
#Fit LDA
lda.fit
#predict using test data
lda.pred <- predict(lda.fit, test)
names(lda.pred)
lda.class <- lda.pred$class
table(lda.class, test.labels)
mean(lda.class == test.labels)
mean(lda.class != test.labels)

lda.fit <- lda(crim_med ~ nox + rad + I(rad^2) + tax + I(tax^2), data = train)
#Fit LDA
lda.fit
#predict using test data
lda.pred <- predict(lda.fit, test)
names(lda.pred)
lda.class <- lda.pred$class
table(lda.class, test.labels)
mean(lda.class == test.labels)
mean(lda.class != test.labels)


qda.fit <- qda(crim_med ~ nox + rad + tax, data = train)
qda.fit
qda.class <- predict(qda.fit, test)$class
table(qda.class, test.labels)
mean(qda.class == test.labels)
mean(qda.class != test.labels)

qda.fit <- qda(crim_med ~ nox + rad + I(rad^2) + tax + I(tax^2), data = train)
qda.fit
qda.class <- predict(qda.fit, test)$class
table(qda.class, test.labels)
mean(qda.class == test.labels)
mean(qda.class != test.labels)

#KNN
set.seed(1)
test_idx <- sample(1:nrow(boston), 51, replace = FALSE) 
standardized.X <- scale(boston[,c(5, 9, 10)])

train.X <- standardized.X[-test_idx,]
test.X <- standardized.X[test_idx,]
train.Y <- boston$crim_med[-test_idx]
test.Y <- boston$crim_med[test_idx]

knn.pred <- knn(train.X, test.X, train.Y, k = 1)
table(knn.pred, test.Y)
mean(knn.pred == test.Y)
mean(knn.pred != test.Y)

knn.pred <- knn(train.X, test.X, train.Y, k = 3)
table(knn.pred, test.Y)
mean(knn.pred == test.Y)
mean(knn.pred != test.Y)

knn.pred <- knn(train.X, test.X, train.Y, k = 5)
table(knn.pred, test.Y)
mean(knn.pred == test.Y)
mean(knn.pred != test.Y)


