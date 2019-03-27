source("load-libraries.R")

#LAB

auto <- as.tibble(Auto)
names(auto) <- tolower(names(auto))
glimpse(auto)

# no idea what is going on with predictions vector
# length shows 392 but display shows 397
predictions <- function(fitted_model) {
    predictions <- predict(fitted_model, auto)
    residuals <- auto$mpg - predictions
    mean(residuals[-train]^2)
}
set.seed(1)
train = sample(392, 196)
lm.fit <- lm(mpg ~ horsepower,
             data = auto,
             subset = train)

predictions(lm.fit)

lm.fit2 <- lm(mpg ~ poly(horsepower, 2),
              data = auto,
              subset = train)
predictions(lm.fit2)

lm.fit3 <- lm(mpg ~ poly(horsepower, 3),
              data = auto,
              subset = train)
predictions(lm.fit3)

# different seed
set.seed(2)
train = sample(392, 196)
lm.fit <- lm(mpg ~ horsepower,
             data = auto,
             subset = train)

predictions(lm.fit)

lm.fit2 <- lm(mpg ~ poly(horsepower, 2),
              data = auto,
              subset = train)
predictions(lm.fit2)

lm.fit3 <- lm(mpg ~ poly(horsepower, 3),
              data = auto,
              subset = train)
predictions(lm.fit3)

#LOOCV
glm.fit <- glm(mpg ~ horsepower, data = auto)
cv.err <- cv.glm(auto, glm.fit)
cv.err$delta

#vectorized
#models_poly <- 1:5 %>% 
#    map(function(x) glm(mpg ~ poly(horsepower, x), data = auto))

cv.error = rep(0, 5)
for (i in 1:5) {
    glm.fit = glm(mpg ~ poly(horsepower, i), data = auto)
    cv.error[i] = cv.glm(auto, glm.fit)$delta[1]
}
cv.error

set.seed(17)
cv.error.10 = rep(0, 10)
for (i in 1:10) {
    glm.fit = glm(mpg ~ poly(horsepower, i), data = auto)
    cv.error[i] = cv.glm(auto, glm.fit, K = 10)$delta[1]
}

## bootstrap
rm(list = ls())
portfolio <- as.tibble(Portfolio)
glimpse(portfolio)

alpha.fn = function(data, index) {
    X = data$X[index]
    Y = data$Y[index]
    return((var(Y) - cov(X, Y))/(var(X) + var(Y) - 2 * cov(X, Y)))
}

set.seed(1)
alpha.fn(portfolio, sample(100, 100, replace = T))
boot(portfolio, alpha.fn, R = 1000)

## Linear model
auto <- as.tibble(Auto)
boot.fn <- function(data, index) 
    return(coef(lm(mpg ~ horsepower, data = data, subset = index)))

boot.fn(auto, 1:392)

set.seed(1)
boot.fn(auto, sample(392, 392, replace = T))
boot.fn(auto, sample(392, 392, replace = T))

boot(auto, boot.fn, 1000)

#FROM formulae
summary(lm(mpg ~ horsepower, data = auto))$coef

boot.fn <- function(data, index) 
    return(coef(lm(mpg ~ horsepower + I(horsepower^2),
                   data = data, subset = index)))
set.seed(1)
boot(auto, boot.fn, 1000)

#FROM formulae
summary(lm(mpg ~ horsepower + I(horsepower^2), data = auto))$coef

#Exercise 2
store = rep(NA, 10000)
for (i in 1:10000) {
    store[i] = sum(sample(1:100, replace = TRUE) == 2) > 0
}
mean(store)

## Exercise 5

defaults <- as.tibble(Default)
defaults

set.seed(1)
glm.fit <- glm(default ~ income + balance, 
               data = defaults, 
               family = binomial)
tidy(glm.fit)
glance(glm.fit)

fit.probs <- predict(glm.fit, type = "response")
fit.preds <- rep("No", length(fit.probs))
fit.preds[fit.probs >= 0.5] <- "Yes"
table(fit.preds, defaults$default)
mean(fit.preds == defaults$default)
mean(fit.preds != defaults$default)

# test_idx <- sample(1:nrow(defaults), nrow(defaults) %/% 2)
# train <- defaults[-test_idx,]
# test <- defaults[test_idx,]
# 
# glm.fit.train <- glm(default ~ income + balance, 
#                      data = train, family = binomial)
# 
# test.probs <- predict(glm.fit.train, test, type = "response")
# test.preds <- rep("No", length(test.probs))
# test.preds[test.probs >= 0.5] <- "Yes"
# table(test.preds, test$default)
# #accuracy
# mean(test.preds == test$default)
# #Error
# mean(test.preds != test$default)

test_performance <- function(vars) {
    test_idx <- sample(1:nrow(defaults), nrow(defaults) %/% 2)
    train <- defaults[-test_idx,]
    test <- defaults[test_idx,]
    
    glm.fit.train <- glm(reformulate(vars, "default"), 
                         data = train, family = binomial)
    
    test.probs <- predict(glm.fit.train, test, type = "response")
    test.preds <- rep("No", length(test.probs))
    test.preds[test.probs >= 0.5] <- "Yes"
    table(test.preds, test$default)
    #accuracy
    mean(test.preds == test$default)
    #Error
    mean(test.preds != test$default)
}

set.seed(1)
features <- c("income", "balance")
test_errors2 <- replicate(3, test_performance(features))
test_errors2
mean(test_errors2)
sd(test_errors2)

features <- c("income", "balance", "student")
test_errors3 <- replicate(3, test_performance(features))
test_errors3
mean(test_errors3)
sd(test_errors)


#EXAMPLE 6
set.seed(1)
glm.fit <- glm(default ~ income + balance, 
               data = defaults, 
               family = binomial)
tidy(glm.fit)
glance(glm.fit)

boot.fn <- function(data, index) 
    return(coef(glm(default ~ income + balance, 
                    data = defaults, family = binomial)))

boot(defaults, boot.fn, 100)

set.seed(1)
r <- replicate(100, boot.fn(auto, sample(10000, 10000, replace = T)))

#EXAMPLE 7
rm(list = ls())
weekly <- as.tibble(Weekly)
names(weekly) <- tolower(names(weekly))
names(weekly)

set.seed(1)
fit_model <- function(i) {
    # i is the observation being held out
    glm.fit <-  glm(direction ~ lag1 + lag2,
                   data = weekly,
                   family = binomial,
                   subset = -(i))
    prob_obs <- predict(glm.fit, weekly[i,], type = "response")
    pred_obs <- if_else(prob_obs >= 0.5, "Up", "Down")
    errors <- if_else(pred_obs != weekly$direction[i], 1, 0)
    return(errors)
}

# call function with the i(th) observation that is being held out
loocv_err <- mean(unlist(map(1:nrow(weekly), fit_model)))
loocv_err

# From library
cost <- function(r, pi = 0) mean(abs(r - pi) > 0.5)
glm.fit = glm(direction ~ lag1 + lag2, data = weekly, family = binomial)
cv.err = cv.glm(weekly, glm.fit, cost = cost)
cv.err$delta

#EXAMPLE 8
rm(list = ls())
set.seed(42)
x = rnorm(100)
y = x - 2*x^2 + rnorm(100)
xy <- tibble(x = x, y = y)

ggplot(data = xy, aes(x = x, y = y)) + 
    geom_point(color = "blue", alpha = 0.6)

# models_poly <- map(1:4, ~lm(reformulate(paste0("poly(", "x", ",", .x, ")"), "y")
#                        , data = xy))
# model_coefs_poly = map_df(models_poly, tidy, .id = "Model")
# model_coefs_poly

# orthogonal vs raw polynomials
# https://stackoverflow.com/questions/29999900/poly-in-lm-difference-between-raw-vs-orthogonal

m1.fit <- glm(y ~ poly(x, 1), data = xy)
cv.err.m1 <- cv.glm(xy, m1.fit)
cv.err.m1$delta
tidy(m1.fit)

m2.fit <- glm(y ~ poly(x, 2), data = xy)
cv.err.m2 <- cv.glm(xy, m2.fit)
cv.err.m2$delta
tidy(m2.fit)

m3.fit <- glm(y ~ poly(x, 3), data = xy)
cv.err.m3 <- cv.glm(xy, m3.fit)
cv.err.m3$delta
tidy(m3.fit)

m4.fit <- glm(y ~ poly(x, 4), data = xy)
cv.err.m4 <- cv.glm(xy, m4.fit)
cv.err.m4$delta
tidy(m4.fit)

## EXERCISE 9
set.seed(1)
rm(list = ls())
boston <- as.tibble(Boston)
names(boston)
mu <- mean(boston$medv)
mu
se <- sd(boston$medv)/sqrt(nrow(boston))
se

boot.fn <- function(data, index) {
    X <-  data$medv[index]
    return(mean(X))
}

bs <- boot(boston, boot.fn, R = 10000)
t.test(Boston$medv)
# summary(bs). t is the vector of bootstrap means. t1 is the "population" mean
# https://stats.idre.ucla.edu/r/faq/how-can-i-generate-bootstrap-statistics-in-r/
CI.bs <- mean(bs$t) + c(-1, 1) * 2 * sd(bs$t)
mean(bs$t)
CI.bs

plot(bs)
bootCI <- boot.ci(boot.out = bs, type = c("norm", "basic", "perc", "bca"))

set.seed(1)
med <- median(boston$medv)
med
boot.med.fn <- function(data, index) {
    X <- data$medv[index]
    return(median(X))
}

bs.med <- boot(boston, boot.med.fn, R = 100000)
plot(bs.med)
bs.med
summary(bs.med)
boot.med.CI <- boot.ci(boot.out = bs.med, type = c("norm", "basic", "perc", "bca"))

set.seed(1)
q10 <- quantile(boston$medv, 0.10)

boot.q10.fn <- function(data, index) {
    X <- data$medv[index]
    return(quantile(X, 0.10))
}

bs.q10 <- boot(boston, boot.q10.fn, R = 10000)
plot(bs.q10)
summary(bs.q10)
