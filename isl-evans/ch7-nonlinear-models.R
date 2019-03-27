source("load-libraries.R")

load.data <- function(data) {
    data <- as.tibble(data)
    head(data)
    glimpse(data)
    return(data)
}

wages <- load.data(Wage)
# orthogonal polynomials
fit1 <- lm(wages$wage ~ poly(age, 4), data = wages)
coef(summary(fit1))
# raw polynomials
fit2 <- lm(wages$wage ~ poly(age, 4, raw = TRUE), data = wages)
coef(summary(fit2))

#create grid and generate predictions
agelims <- range(wages$age)
age.grid <- seq(from = agelims[1], to = agelims[2])
head(age.grid)
preds <- predict(fit1, newdata = list(age = age.grid), se = TRUE)
se.bands <- cbind(preds$fit + 2 * preds$se.fit, preds$fit - 2 * preds$se.fit)

# par(mfrow = c(1,2),mar = c(4.5,4.5,1,1) ,oma = c(0,0,4,0))
# plot(wages$age, wages$wage,xlim = agelims ,cex = .5, col = "darkgrey")
# lines(age.grid,preds$fit,lwd = 2,col = "blue")
# matlines(age.grid,se.bands,lwd = 1,col = "blue",lty = 3)

ggplot() +
    geom_point(data = wages, aes(x = wages$age, y = wages$wage), 
               alpha = 0.3, color = "dodgerblue") +
    geom_line(aes(x = age.grid, y = preds$fit), color = "red") +
    geom_ribbon(aes(x = age.grid, ymin = se.bands[,2], ymax = se.bands[,1]), 
                fill = "grey", alpha = 0.6) +
    labs(x = "age", y = "wage", title = "Degree (4) Polynomial")

# create models with degrees 1:5
models <- map(1:5, function(degree) lm(wage ~ poly(age, degree), data = wages))
# test usefulness of additional complexity using ANOVA
anova.fit <- anova(models[[1]], models[[2]], models[[3]], models[[4]], models[[5]])
anova.fit

# instead of anova, can look at the coefs from the poly(age, 5) model
coef(summary(models[[5]]))

# Logistic
fit <- glm(I(wage > 250) ~ poly(age, 4), data = wages, family = binomial)
preds <- predict(fit, newdata = list(age = age.grid), se = TRUE)
# can also use type = response to get prob predictions
pfit <- exp(preds$fit)/(1 + exp(preds$fit))
se.bands.logit <- cbind(preds$fit + 2 * preds$se.fit, 
                        preds$fit - 2 * preds$se.fit)
se.bands <- exp(se.bands.logit)/(1 + exp(se.bands.logit))

ggplot() +
    geom_point(aes(x = age.grid, y = pfit), color = "green") +
    geom_ribbon(aes(x = age.grid[1:57], ymin = se.bands[1:57,2], ymax = se.bands[1:57,1]),
                fill = "grey", alpha = 0.6)

# step function
table(with(wages, cut(age, 4)))
fit <- lm(wage ~ cut(age, 4), data = wages)
coef(summary(fit))

# SPLINES
fit <- lm(wage ~ bs(age, knots = c(25, 40, 60)), data = wages)
pred <- predict(fit, newdata = list(age = age.grid), se = TRUE)
plot(wages$age, wages$wage, col = "grey")
lines(age.grid, pred$fit, lwd = 1)
lines(age.grid, pred$fit + 2 * pred$se, lty = "dashed")
lines(age.grid, pred$fit - 2 * pred$se, lty = "dashed")

ggp_splines <- ggplot() +
    geom_point(data = wages, aes(x = age, y = wage), 
               alpha = 0.3, color = "darkolivegreen4") +
    geom_line(aes(x = age.grid, y = pred$fit), color = "purple4", alpha = 0.7) +
    geom_ribbon(aes(x = age.grid, 
                    ymin = pred$fit - 2 * pred$se,
                    ymax = pred$fit + 2 * pred$se), fill = "ivory", alpha = 0.5)
dim(bs(wages$age, knots = c(25, 40, 60)))
dim(bs(wages$age, df = 6))
attr(bs(wages$age,df = 6),"knots")

#natural spline
fit2 <- lm(wage ~ ns(age, df = 4), data = wages)
pred2 <- predict(fit2, newdata = list(age = age.grid), se = TRUE)
lines(age.grid, pred2$fit, col = "red", lwd = 2)

ggp_splines + geom_line(aes(x = age.grid, y = pred2$fit), 
                        color = "salmon3", alpha = 0.7)

#smoothing spline

fit <- smooth.spline(wages$age, wages$wage, df = 16)
fit2 <- smooth.spline(wages$age, wages$wage, cv = TRUE) # with cross validation
fit2$df
plot(wages$age, wages$wage, xlim = agelims, cex = .5, col = "salmon")
lines(fit, col = "darkolivegreen2", lwd = 2)
lines(fit2, col = "orchid", lwd = 2)

# local regression
fit <- loess(wage ~ age, span = 0.2, data = wages) # 20% of observations
fit2 <- loess(wage ~ age, span = 0.5, data = wages) # 50% of observations
plot(wages$age, wages$wage, xlim = agelims, cex = .5, col = "gold4")
lines(age.grid,predict(fit, data.frame(age = age.grid)), 
      col = "dodgerblue",lwd = 2)
lines(age.grid,predict(fit2, data.frame(age = age.grid)), 
      col = "orchid",lwd = 2)      

### GAM
# fit natural splines to age and year with education as qualitative predictor
gam1 <- lm(wage ~ ns(year, 4) + ns(age, 5) + education, data = wages)

# fit smoothing spline
gam.m3 <- gam(wage ~ s(year, 4) + s(age, 5) + education, 
              data = wages)

dev.off()
par(mfrow = c(1, 3))
plot.Gam(gam.m3, se = TRUE, col = "dodgerblue")
plot.Gam(gam1, se = TRUE, col = "orchid")

gam.m1 <- gam(wage ~ s(age, 5) + education, data = wages) # no year
gam.m2 <- gam(wage ~ year + s(age, 5) + education, data = wages) # year linesr
anova(gam.m1, gam.m2, gam.m3)

summary(gam.m3)

# generating predictions from GAM 
preds <- predict(gam.m2, newtest = wages)

# use local regression fits inside GAM
gam.lo <- gam(wage ~ s(year, df = 4) + lo(age, span = 0.7) + education, 
              data = wages)
plot.Gam(gam.lo, col = "turquoise4")

# create interactions
gam.lo.i <- gam(wage ~ lo(year, age, span = 0.5) + education, data = wages)
plot(gam.lo.i)

gam.lr <- gam(I(wage > 250) ~ year + s(age, df = 5) + education, 
              family = binomial, data = wages)
par(mfrow = c(1, 3))
plot(gam.lr, se = TRUE, col = "dodgerblue")

with(wages, table(education, I(wage > 250)))

gam.lr.s <- gam(I(wage > 250) ~ year + s(age, df = 5) + education, 
              family = binomial, data = wages, 
              subset = (education != "1. < HS Grad"))
plot(gam.lr.s, se = TRUE, col = "firebrick4")


### Exercise 6 #########

# fits the polynomial regression of degree = deg
# conducts cross validation errors for each model, folds K = 5
# returns cross validation error for each degree
set.seed(1)
cv.errors.deg <- function(deg) {
    cv.glm(wages, glm(wage ~ poly(age, deg), data = wages), K = 5)$delta[1]
}

# cross-validation of polynomial regression (degrees 1 to 15)
# map calls cv.errors.deg with each degree from 1 to 15
cv.errors <- unlist(map(1:15, cv.errors.deg))
which.min(cv.errors) # 4 is selected

# plot of cv errors versus polynomial degree
qplot(x = 1:15, y = cv.errors)

poly.min.fit <- glm(wage ~ poly(age, which.min(cv.errors)), data = wages)
pred.poly.4 <- predict(poly.min.fit, newdata = list(age = age.grid), se = TRUE)
se.bands <- cbind(pred.poly.4$fit + 2 * pred.poly.4$se.fit, 
                  pred.poly.4$fit - 2 * pred.poly.4$se.fit)

gg_wages <- ggplot() +
    geom_point(data = wages, aes(x = age, y = wage),
               color = "darksalmon", alpha = 0.8)
gg_wages + 
    geom_line(aes(x = age.grid, y = pred.poly.4$fit), 
                     color = "turquoise2") +
    geom_ribbon(aes(x = age.grid, 
                    ymin = se.bands[,2], ymax = se.bands[,1]), alpha = 0.1)

## Step function. Use cross validation to determine number of cuts

set.seed(1)
cv.opt.cuts <- function(ncuts) {
    wages$age.cut <- cut(wages$age, ncuts)
    cv.glm(wages, glm(wage ~ age.cut, data = wages), K = 5)$delta[1]
}

opt.cuts <- unlist(map(2:10, cv.opt.cuts))
which.min(opt.cuts) + 1 # cuts start at 2: selects 8
qplot(x = 2:10, opt.cuts)

# fit model with selected cut
fit.cut.opt <- lm(wage ~ cut(age, which.min(opt.cuts) + 1), data = wages)
tidy(fit.cut.opt)
glance(fit.cut.opt)
wages.aug <- augment(fit.cut.opt, newdata = tibble(age = age.grid))
se.bands <- cbind(wages.aug$.fitted + 2 * wages.aug$.se.fit,
                  wages.aug$.fitted - 2 * wages.aug$.se.fit)

ggplot() +
    geom_point(data = wages, aes(x = age, y = wage), 
               color = "darkseagreen2") +
    geom_line(aes(x = wages.aug$age, wages.aug$.fitted), 
              color = "dodgerblue4") +
    geom_ribbon(aes(x = wages.aug$age, ymin = se.bands[,2], ymax = se.bands[,1]),
                fill = "honeydew1", alpha = 0.7)

## end model with cuts: step function

wages %>%
    gather(-year, -age, -wage, key = "var", value = "value")

wages <- load.data(Wage)
# correlation one variable against all others

# keep only numeric columns
num_wages <- wages %>% select_if(is.numeric)
# use apply to get correlations of wage against numeric variables
apply(num_wages, 2, function(colms) cor(colms, num_wages$wage))

wages %>% group_by(maritl) %>% summarise(mean(wage))
gg_maritl <- wages %>% ggplot(aes(x = maritl, y = wage)) +
    geom_boxplot(fill = "yellowgreen")
gg_maritl +  geom_jitter(width = 0.1, alpha = 0.3)

# recoding factor
# wages %>% 
#     mutate(maritl_x = as.character(maritl)) %>% 
#     mutate(maritl_x = if_else(maritl %in% c("3. Widowed", 
#                                             "4. Divorced", 
#                                             "5. Separated"), 
#                               "0. Other", maritl_x)) %>%
#     mutate(maritl_x = as.factor(maritl_x)) 

# easier to use recode_factor command
# wages %>%
#     mutate(maritlx = as.character(maritl)) %>%
#     mutate(maritlx = if_else(maritl != "2. Married", "O. Other", maritlx)) %>%
#     ggplot(aes(x = maritlx, y = wage)) +
#     geom_violin(scale = "count", fill = "gold") # scale proportional to sample size

wages %>%
    mutate(maritlx = recode_factor(maritl, 
                                   "1. Never Married" = "0. Other",
                                   "3. Widowed" = "0. Other", 
                                   "4. Divorced" = "0. Other", 
                                   "5. Separated" = "0. Other")) %>%
    ggplot(aes(x = maritlx, y = wage)) +
    geom_violin(scale = "count", fill = "gold") # scale proportional to sample size
    
wages %>% group_by(race) %>% summarise(mean(wage))
gg_race <- wages %>% ggplot(aes(x = race, y = wage))
gg_race + geom_boxplot(fill = "salmon")
gg_race + geom_violin(scale = "count", fill = "salmon")

wages %>% group_by(education) %>% summarise(mean(wage))
gg_educ <- wages %>% ggplot(aes(x = education, y = wage))
gg_educ + geom_boxplot(fill = "steelblue1")
gg_educ + geom_violin(scale = "count", fill = "steelblue1")

# wagesx calculated below
gg_educ <- wagesx %>% ggplot(aes(x = education, y = wage))
gg_educ + geom_violin(aes(fill = factor(maritlx)), scale = "count")

# only one region is sampled
wages %>% group_by(region) %>% summarise(mean(wage))

wages %>% group_by(jobclass) %>% summarise(mean(wage))
gg_jclass <- wages %>% ggplot(aes(x = jobclass, y = wage))
gg_jclass + geom_boxplot(fill = "paleturquoise")
gg_jclass + geom_violin(scale = "count", fill = "paleturquoise")

gg_jclass + geom_violin(aes(fill = factor(education)), scale = "count")


wages %>% group_by(health) %>% summarise(mean(wage))
gg_health <- wages %>% ggplot(aes(x = health, y = wage))
gg_health + geom_boxplot(fill = "aliceblue")
gg_health + geom_violin(scale = "count", fill = "aliceblue")

wages %>% group_by(health_ins) %>% summarise(mean(wage))
gg_ins <- wages %>% ggplot(aes(x = health_ins, y = wage))
gg_ins + geom_boxplot(fill = "blanchedalmond")
gg_ins + geom_violin(scale = "count", fill = "blanchedalmond")

wages %>% group_by(jobclass, education) %>% mean(wage)

## Using GAM
# natural splines
gam.fit.11 <- lm(wage ~ ns(age, 5) + maritl, data = wages)

# recode maritl
wagesx <- wages %>%
    mutate(maritlx = recode_factor(maritl, 
                                   "1. Never Married" = "0. Other",
                                   "3. Widowed" = "0. Other", 
                                   "4. Divorced" = "0. Other", 
                                   "5. Separated" = "0. Other"))

gam.fit.12 <- lm(wage ~ ns(age, 5) + maritlx, data = wagesx)
plot(gam.fit.12)

# smoothing splines
gam.fit.21 <- gam(wage ~ s(age, 5) + maritlx, data = wagesx)
dev.off()
par(mfrow = c(1, 3))
plot(gam.fit.21, se = TRUE, col = "dodgerblue2")

gam.fit.22 <- gam(wage ~ s(age, 5) + race, data = wagesx)
plot(gam.fit.22, se = TRUE, col = "darkorange")

wages %>% group_by(education, race) %>% summarise(mean(wage), n())
wages %>% group_by(race, education) %>% summarise(mean(wage), n())

gam.fit.23 <- gam(wage ~ s(age, 5) + jobclass, data = wagesx)
plot(gam.fit.23, se = TRUE, col = "darkolivegreen4")

gam.fit.24 <- gam(wage ~ s(age, 5) + education, data = wagesx)
plot(gam.fit.24, se = TRUE, col = "salmon")

gam.fit.30 <- gam(wage ~ s(age, 5), data = wagesx)
gam.fit.31 <- gam(wage ~ s(age, 5) + education + race, data = wagesx)
gam.fit.32 <- gam(wage ~ s(age, 5) + education + race + maritlx, data = wagesx)
gam.fit.33 <- gam(wage ~ s(age, 5) + education + race + maritlx + jobclass, 
                  data = wagesx)
anova(gam.fit.30, gam.fit.24, gam.fit.31, gam.fit.32, gam.fit.33)
summary(gam.fit.33)
preds <- predict(gam.fit.33, newdata = wagesx)
resids <- wagesx$wage - preds
qplot(preds, resids)
# use local regression
gam.fit.41 <- gam(wage ~ lo(age, 0.7) + education + race + maritlx + jobclass, 
                  data = wagesx)

## Example 8 #################################################
rm(list = ls())
auto <-load.data(Auto)
auto <- auto %>% mutate(origin = as.factor(origin), 
                        cylinders = as.factor(cylinders))
glimpse(auto)

qplot(displacement, mpg, color = cylinders, data = auto)
qplot(horsepower, mpg, color = cylinders, data = auto)
qplot(weight, mpg, color = cylinders, data = auto)
qplot(acceleration, mpg, color = cylinders, data = auto)

qplot(displacement, mpg, color = origin, data = auto)
qplot(horsepower, mpg, color = origin, data = auto)
qplot(weight, mpg, color = origin, data = auto)
qplot(acceleration, mpg, color = origin, data = auto)

qplot(year, mpg, data = auto, color = origin)

table(auto$year, auto$origin)
table(auto$year, auto$cylinders)

# recode 3-4: 34, 5-6-8: 568
autox <- auto %>% 
    mutate(cylindersx = recode_factor(cylinders,
                                      "3" = 34,
                                      "4" = 34,
                                      "5" = 568,
                                      "6" = 568,
                                      "8" = 568
    ))
table(autox$cylindersx, autox$cylinders)

qplot(displacement, mpg, color = cylindersx, data = autox)
qplot(horsepower, mpg, color = cylindersx, data = autox)
qplot(weight, mpg, color = cylindersx, data = autox)
qplot(acceleration, mpg, color = cylindersx, data = autox)

# polynomial regression
# fit different degrees 
# fit different models
# use cross-validation
# weight -------------------


wgt.lims <- range(autox$weight)
wgt.grid <- seq(from = wgt.lims[1], to = wgt.lims[2])

cv.autox.deg <- function(deg) {
    cv.glm(autox, glm(mpg ~ poly(weight, deg), data = autox))$delta[1]
}

set.seed(1)
cv.autox.err1 <- unlist(map(1:5, cv.autox.deg))
min.deg <- which.min(cv.autox.err1) # 4 is selected
qplot(c(1:5), cv.autox.err1)

# fit poly with deg chosen by cv
poly.fit.min.wgt <- glm(mpg ~ poly(weight, min.deg), data = autox)
tidy(poly.fit.min.wgt)
# predict at grid values
preds.poly.wgt <- predict(poly.fit.min.wgt, newdata = list(weight = wgt.grid), 
                          se = TRUE)
se.bands <- cbind(preds.poly.wgt$fit + 2 * preds.poly.wgt$se.fit,
                  preds.poly.wgt$fit - 2 * preds.poly.wgt$se.fit)
ggplot() +
    geom_point(data = autox, aes(x = weight, y = mpg), 
               alpha = 0.5, color = "purple") +
    geom_line(aes(x = wgt.grid, y = preds.poly.wgt$fit)) +
    geom_ribbon(aes(x = wgt.grid, ymin = se.bands[,2], ymax = se.bands[,1]),
                alpha = 0.3) 

# end of weight -------------------
# horsepower
hp.lims <- range(autox$horsepower)
hp.grid <- seq(from = hp.lims[1], to = hp.lims[2])

cv.autox.deg <- function(deg) {
    cv.glm(autox, glm(mpg ~ poly(horsepower, deg), data = autox))$delta[1]
}

set.seed(1)
cv.autox.err2 <- unlist(map(1:10, cv.autox.deg))
min.deg <- which.min(cv.autox.err2) 
qplot(c(1:10), cv.autox.err2)

# fit poly with deg chosen by cv
poly.fit.min.hp <- glm(mpg ~ poly(horsepower, min.deg), data = autox)
tidy(poly.fit.min.hp)
# predict at grid values
preds.poly.hp <- predict(poly.fit.min.hp, newdata = list(horsepower = hp.grid), 
                          se = TRUE)
se.bands <- cbind(preds.poly.hp$fit + 2 * preds.poly.hp$se.fit,
                  preds.poly.hp$fit - 2 * preds.poly.hp$se.fit)
ggplot() +
    geom_point(data = autox, aes(x = horsepower, y = mpg), 
               alpha = 0.5, color = "salmon") +
    geom_line(aes(x = hp.grid, y = preds.poly.hp$fit)) +
    geom_ribbon(aes(x = hp.grid, ymin = se.bands[,2], ymax = se.bands[,1]),
                alpha = 0.3)

# end of horsepower -------------------
# displacement
disp.lims <- range(autox$displacement)
disp.grid <- seq(from = disp.lims[1], to = disp.lims[2])

cv.autox.deg <- function(deg) {
    cv.glm(autox, glm(mpg ~ poly(displacement, deg), data = autox))$delta[1]
}

set.seed(1)
cv.autox.err3 <- unlist(map(1:15, cv.autox.deg))
min.deg <- which.min(cv.autox.err3) 
qplot(c(1:15), cv.autox.err3)

# fit poly with deg chosen by cv
poly.fit.min.disp <- glm(mpg ~ poly(displacement, min.deg), data = autox)
tidy(poly.fit.min.disp)
# predict at grid values
preds.poly.disp <- predict(poly.fit.min.disp, 
                           newdata = list(displacement = disp.grid), 
                           se = TRUE)
se.bands <- cbind(preds.poly.disp$fit + 2 * preds.poly.disp$se.fit,
                  preds.poly.disp$fit - 2 * preds.poly.disp$se.fit)
ggplot() +
    geom_point(data = autox, aes(x = displacement, y = mpg), 
               alpha = 0.5, color = "darkcyan") +
    geom_line(aes(x = disp.grid, y = preds.poly.disp$fit)) +
    geom_ribbon(aes(x = disp.grid, ymin = se.bands[,2], ymax = se.bands[,1]),
                alpha = 0.3)

# end of horsepower -------------------
# END of polynomial regressions

# Regression splines
# df = 6, cubic spline
# weight
fit.rgspl.wgt <- lm(mpg ~ bs(weight, df = 6), data = autox)
tidy(fit.rgspl.wgt)
pred.rgspl.wgt <- predict(fit.rgspl.wgt, newdata = list(weight = wgt.grid), 
                          se = TRUE)
se.bands <- cbind(pred.rgspl.wgt$fit + 2 * pred.rgspl.wgt$se.fit,
                  pred.rgspl.wgt$fit - 2 * pred.rgspl.wgt$se.fit)
ggplot() +
    geom_point(data = autox, aes(x = weight, y = mpg), 
               alpha = 0.5, color = "cornflowerblue") +
    geom_line(aes(x = wgt.grid, y = pred.rgspl.wgt$fit)) +
    geom_ribbon(aes(x = wgt.grid, ymin = se.bands[,2], ymax = se.bands[,1]),
                alpha = 0.3)

## displacement
fit.rgspl.disp <- lm(mpg ~ bs(displacement, df = 6), data = autox)
tidy(fit.rgspl.disp)
pred.rgspl.disp <- predict(fit.rgspl.disp, 
                           newdata = list(displacement = disp.grid), 
                          se = TRUE)
se.bands <- cbind(pred.rgspl.disp$fit + 2 * pred.rgspl.disp$se.fit,
                  pred.rgspl.disp$fit - 2 * pred.rgspl.disp$se.fit)
ggplot() +
    geom_point(data = autox, aes(x = displacement, y = mpg), 
               alpha = 0.5, color = "cornsilk4") +
    geom_line(aes(x = disp.grid, y = pred.rgspl.disp$fit),
              color = "coral4") +
    geom_ribbon(aes(x = disp.grid, ymin = se.bands[,2], ymax = se.bands[,1]),
                alpha = 0.3)

## horsepower
fit.rgspl.hp <- lm(mpg ~ bs(horsepower, df = 6), data = autox)
tidy(fit.rgspl.hp)
pred.rgspl.hp <- predict(fit.rgspl.hp, 
                           newdata = list(horsepower = hp.grid), 
                           se = TRUE)
se.bands <- cbind(pred.rgspl.hp$fit + 2 * pred.rgspl.hp$se.fit,
                  pred.rgspl.hp$fit - 2 * pred.rgspl.hp$se.fit)
ggplot() +
    geom_point(data = autox, aes(x = horsepower, y = mpg), 
               alpha = 0.5, color = "deepskyblue") +
    geom_line(aes(x = hp.grid, y = pred.rgspl.hp$fit),
              color = "dodgerblue") +
    geom_ribbon(aes(x = hp.grid, ymin = se.bands[,2], ymax = se.bands[,1]),
                alpha = 0.2)

## Fit Natural Splines
fit.ns.wgt <- glm(mpg ~ ns(weight, df = 4), data = autox)
tidy(fit.ns.wgt)
pred.ns.wgt <- predict(fit.ns.wgt, 
                         newdata = list(weight = wgt.grid), 
                         se = TRUE)
se.bands <- cbind(pred.ns.wgt$fit + 2 * pred.ns.wgt$se.fit,
                  pred.ns.wgt$fit - 2 * pred.ns.wgt$se.fit)
ggplot() +
    geom_point(data = autox, aes(x = weight, y = mpg), 
               alpha = 0.5, color = "cornflowerblue") +
    geom_line(aes(x = wgt.grid, y = pred.ns.wgt$fit)) +
    geom_ribbon(aes(x = wgt.grid, ymin = se.bands[,2], ymax = se.bands[,1]),
                alpha = 0.3)

## horsepower
fit.ns.hp <- lm(mpg ~ ns(horsepower, df = 4), data = autox)
tidy(fit.ns.hp)
pred.ns.hp <- predict(fit.ns.hp, 
                       newdata = list(horsepower = hp.grid), 
                       se = TRUE)
se.bands <- cbind(pred.ns.hp$fit + 2 * pred.ns.hp$se.fit,
                  pred.ns.hp$fit - 2 * pred.ns.hp$se.fit)
ggplot() +
    geom_point(data = autox, aes(x = horsepower, y = mpg), 
               alpha = 0.5, color = "darkslategrey") +
    geom_line(aes(x = hp.grid, y = pred.ns.hp$fit)) +
    geom_ribbon(aes(x = hp.grid, ymin = se.bands[,2], ymax = se.bands[,1]),
                alpha = 0.3)

## displacement
fit.ns.disp <- glm(mpg ~ ns(displacement, df = 4), data = autox)
tidy(fit.ns.disp)
pred.ns.disp <- predict(fit.ns.disp, 
                      newdata = list(displacement = disp.grid), 
                      se = TRUE)
se.bands <- cbind(pred.ns.disp$fit + 2 * pred.ns.disp$se.fit,
                  pred.ns.disp$fit - 2 * pred.ns.disp$se.fit)
ggplot() +
    geom_point(data = autox, aes(x = displacement, y = mpg), 
               alpha = 1.0, color = "greenyellow") +
    geom_line(aes(x = disp.grid, y = pred.ns.disp$fit)) +
    geom_ribbon(aes(x = disp.grid, ymin = se.bands[,2], ymax = se.bands[,1]),
                alpha = 0.15)

## Find df through cross-validation
## weight
cv.autox.df <- function(dfree) {
    cv.glm(autox, 
           glm(mpg ~ ns(weight, df = dfree), data = autox),
           K = 5)$delta[1]
}

set.seed(1)
ns.wgt.err <- unlist(map(1:15, cv.autox.df))
ns.min.deg <- which.min(ns.wgt.err)
qplot(c(1:15), ns.wgt.err)

fit.ns.wgt <- glm(mpg ~ ns(weight, df = ns.min.deg), data = autox)
tidy(fit.ns.wgt)
pred.ns.wgt <- predict(fit.ns.wgt, 
                       newdata = list(weight = wgt.grid), 
                       se = TRUE)
se.bands <- cbind(pred.ns.wgt$fit + 2 * pred.ns.wgt$se.fit,
                  pred.ns.wgt$fit - 2 * pred.ns.wgt$se.fit)
ggplot() +
    geom_point(data = autox, aes(x = weight, y = mpg), 
               alpha = 0.5, color = "mediumvioletred") +
    geom_line(aes(x = wgt.grid, y = pred.ns.wgt$fit)) +
    geom_ribbon(aes(x = wgt.grid, ymin = se.bands[,2], ymax = se.bands[,1]),
                alpha = 0.3)

## horsepower
cv.autox.df <- function(dfree) {
    cv.glm(autox, 
           glm(mpg ~ ns(horsepower, df = dfree), data = autox),
           K = 5)$delta[1]
}

set.seed(1)
ns.hp.err <- unlist(map(1:15, cv.autox.df))
ns.min.deg <- which.min(ns.hp.err)
qplot(c(1:15), ns.hp.err)

fit.ns.hp <- lm(mpg ~ ns(horsepower, df = ns.min.deg), data = autox)
tidy(fit.ns.hp)
pred.ns.hp <- predict(fit.ns.hp, 
                      newdata = list(horsepower = hp.grid), 
                      se = TRUE)
se.bands <- cbind(pred.ns.hp$fit + 2 * pred.ns.hp$se.fit,
                  pred.ns.hp$fit - 2 * pred.ns.hp$se.fit)
ggplot() +
    geom_point(data = autox, aes(x = horsepower, y = mpg), 
               alpha = 0.7, color = "navyblue") +
    geom_line(aes(x = hp.grid, y = pred.ns.hp$fit),
              color = "dodgerblue2") +
    geom_ribbon(aes(x = hp.grid, ymin = se.bands[,2], ymax = se.bands[,1]),
                alpha = 0.3)

# displacement
cv.autox.df <- function(dfree) {
    cv.glm(autox, 
           glm(mpg ~ ns(displacement, df = dfree), data = autox),
           K = 5)$delta[1]
}

set.seed(1)
ns.disp.err <- unlist(map(1:15, cv.autox.df))
ns.min.deg <- which.min(ns.disp.err)
qplot(c(1:15), ns.disp.err)

fit.ns.disp <- glm(mpg ~ ns(displacement, df = ns.min.deg), data = autox)
tidy(fit.ns.disp)
pred.ns.disp <- predict(fit.ns.disp, 
                        newdata = list(displacement = disp.grid), 
                        se = TRUE)
se.bands <- cbind(pred.ns.disp$fit + 2 * pred.ns.disp$se.fit,
                  pred.ns.disp$fit - 2 * pred.ns.disp$se.fit)
ggplot() +
    geom_point(data = autox, aes(x = displacement, y = mpg), 
               alpha = 1.0, color = "greenyellow") +
    geom_line(aes(x = disp.grid, y = pred.ns.disp$fit),
              color = "grey") +
    geom_ribbon(aes(x = disp.grid, ymin = se.bands[,2], ymax = se.bands[,1]),
                alpha = 0.15)

# acceleration
cv.autox.df <- function(dfree) {
    cv.glm(autox, 
           glm(mpg ~ ns(acceleration, df = dfree), data = autox),
           K = 5)$delta[1]
}

set.seed(1)
ns.disp.err <- unlist(map(1:15, cv.autox.df))
ns.min.deg <- which.min(ns.disp.err)
qplot(c(1:15), ns.disp.err)

## END Natural Splines

## Fit Smoothing Splines
# weight
x <- autox$weight
y <- autox$mpg
fit.smth.wgt <- smooth.spline(x, y, cv = TRUE)
plot(x, y)
lines(fit.smth.wgt, col = "red", lwd = 1)
fit.smth.wgt$df
# OR
preds.smth.wgt <- predict(fit.smth.wgt, x = wgt.grid)
ggplot() + 
    geom_point(data = autox, aes(x = weight, y = mpg), 
                      color = "cornsilk4") +
    geom_line(aes(preds.smth.wgt$x, preds.smth.wgt$y), color = "green")
    
# horsepower
x <- autox$horsepower
y <- autox$mpg
fit.smth.hp <- smooth.spline(x, y, cv = TRUE)
plot(x, y)
lines(fit.smth.hp, col = "dodgerblue", lwd = 2)
fit.smth.hp$df
# OR
preds.smth.hp <- predict(fit.smth.hp, x = hp.grid)
ggplot() + 
    geom_point(data = autox, aes(x = horsepower, y = mpg), 
               color = "darkslategray1") +
    geom_line(aes(preds.smth.hp$x, preds.smth.hp$y), color = "blue")


# displacement
x <- autox$displacement
y <- autox$mpg
fit.smth.disp <- smooth.spline(x, y, cv = TRUE)
plot(x, y)
lines(fit.smth.disp, col = "chocolate", lwd = 2)
fit.smth.disp$df
# OR
preds.smth.disp <- predict(fit.smth.disp, x = disp.grid)
ggplot() + 
    geom_point(data = autox, aes(x = displacement, y = mpg), 
               color = "cornsilk4") +
    geom_line(aes(preds.smth.disp$x, preds.smth.disp$y), color = "green")

## acceleration
x <- autox$acceleration
y <- autox$mpg
fit.smth.disp <- smooth.spline(x, y, cv = TRUE)
plot(x, y)
lines(fit.smth.disp, col = "salmon", lwd = 2)
fit.smth.disp$df

## LOESS: local regression
# weight
lo.fit.wgt1 <- loess(mpg ~ weight, span = 0.2, data = autox)
lo.fit.wgt2 <- loess(mpg ~ weight, span = 0.5, data = autox)
plot(autox$weight, autox$mpg)
lines(wgt.grid, predict(lo.fit.wgt1, data.frame(weight = wgt.grid)), 
      col = "red")
lines(wgt.grid, predict(lo.fit.wgt2, data.frame(weight = wgt.grid)), 
      col = "green")
# OR
preds.lo.wgt1 <- predict(lo.fit.wgt1, newdata = wgt.grid, se = TRUE)
preds.lo.wgt2 <- predict(lo.fit.wgt2, newdata = wgt.grid, se = TRUE)

ggplot() + 
    geom_point(data = autox, aes(x = weight, y = mpg), 
               color = "cornsilk4") +
    geom_line(aes(wgt.grid, preds.lo.wgt1$fit), color = "green") +
    geom_line(aes(wgt.grid, preds.lo.wgt2$fit), color = "purple")

#horsepower
lo.fit.hp1 <- loess(mpg ~ horsepower, span = 0.2, data = autox)
lo.fit.hp2 <- loess(mpg ~ horsepower, span = 0.7, data = autox)
plot(autox$horsepower, autox$mpg)
lines(hp.grid, predict(lo.fit.hp1, data.frame(horsepower = hp.grid)), 
      col = "red")
lines(hp.grid, predict(lo.fit.hp2, data.frame(horsepower = hp.grid)), 
      col = "green")

preds.lo.hp1 <- predict(lo.fit.hp1, newdata = hp.grid, se = TRUE)
preds.lo.hp2 <- predict(lo.fit.hp2, newdata = hp.grid, se = TRUE)

ggplot() + 
    geom_point(data = autox, aes(x = horsepower, y = mpg), 
               color = "cornsilk4") +
    geom_line(aes(hp.grid, preds.lo.hp1$fit), color = "green") +
    geom_line(aes(hp.grid, preds.lo.hp2$fit), color = "purple")

#displacement
lo.fit.disp1 <- loess(mpg ~ displacement, span = 0.2, data = autox)
lo.fit.disp2 <- loess(mpg ~ displacement, span = 0.7, data = autox)
plot(autox$displacement, autox$mpg)
lines(disp.grid, predict(lo.fit.disp1, data.frame(displacement = disp.grid)), 
      col = "red")
lines(disp.grid, predict(lo.fit.disp2, data.frame(displacement = disp.grid)), 
      col = "green")

preds.lo.disp1 <- predict(lo.fit.disp1, newdata = disp.grid, se = TRUE)
preds.lo.disp2 <- predict(lo.fit.disp2, newdata = disp.grid, se = TRUE)

ggplot() + 
    geom_point(data = autox, aes(x = displacement, y = mpg), 
               color = "cornsilk4") +
    geom_line(aes(disp.grid, preds.lo.disp1$fit), color = "green") +
    geom_line(aes(disp.grid, preds.lo.disp2$fit), color = "purple")

## GAMs
gam0 <- lm(mpg ~ ns(weight, 7) + ns(acceleration, 8) + cylindersx, data = autox)
par(mfrow = c(1, 3))
plot.Gam(gam0, se = TRUE, col = "chocolate")
tidy(gam0)
## smoothing splines
gam2 <- gam(mpg ~ s(weight, 10) + s(acceleration, 5) + cylindersx, data = autox)
par(mfrow = c(1, 3))
plot(gam2, se = TRUE, col = "dodgerblue2")

gam1 <- gam(mpg ~ s(weight, 10) + cylindersx, data = autox)
gam3 <- gam(mpg ~ s(weight, 10) + s(acceleration, 5) +
                s(horsepower, 6) + cylindersx, data = autox)
anova(gam1, gam2, gam3, test = "F")
par(mfrow = c(2, 2))
plot(gam3, se = TRUE, col = "deepskyblue")

# predictions on training set
preds.gam = predict(gam3, newdata = autox)
resid.gam = autox$mpg - preds.gam
qplot(preds.gam, resid.gam)

# Discussion of GAM as implemented in R
# See use of the predict function to obtain term contributions
# http://www.win-vector.com/blog/2011/02/the-cranky-guide-to-trying-r-packages/
d <- tibble(
    x1=rnorm(100),
    x2=sample(100),
    x3=0*(1:100),
    x4=sample(c('a','b','c'),size=100,replace=T),
    x5=as.factor(sample(c('d','e','f'),size=100,replace=T)),
    x6=sample(c(F,T),size=100,replace=T),
    x7=NA + 1:100)

d$x8 = d$x1
d$y = rnorm(100) + with(d,20*exp(x1) + 
                            x2 + 7*as.integer(as.factor(x4)) + 
                            9*as.integer(x5) + 10*as.integer(x6))
m1 <- lm(y ~ x1 + x2 + x3 + x4 + x5 + x6 + x8, data=d)
ggplot(d,aes(predict(m1),y)) + 
    geom_point(shape=1, color = "dodgerblue3") + 
    geom_abline(slope=1, color = "chocolate")

m2 <- lm(y ~ exp(x1) + x2 + x3 + x4 + x5 + x6 + x8, data=d)

ggplot(d,aes(predict(m2),y)) + 
    geom_point(shape=1, color = "dodgerblue3") + 
    geom_abline(slope=1, color = "chocolate")

mG <- gam(y ~ s(x1) + s(x2) + x3 + x4 + x5 + x6 + s(x8), data=d)
ggplot(d,aes(predict(mG),y)) + 
    geom_point(shape=1, color = "dodgerblue3") + 
    geom_abline(slope=1, color = "chocolate")

pG = predict(mG,type='terms')
summary(lm(d$y ~ pG))

ggplot(d) + geom_point(aes(x8,pG[,'s(x8)']), color = "chocolate")
ggplot(d) + geom_point(aes(x1,pG[,'s(x1)']), color = "chocolate")
ggplot(d) + geom_point(aes(exp(d$x1),pG[,'s(x8)'] + pG[,'s(x1)']), 
                       color = "chocolate") 

var(pG[,'s(x1)'])
var(pG[,'s(x8)'])
coef(mG)

## applying the above to the autox gam model
pg3 <- predict(gam3, type = "terms")
summary(lm(autox$mpg ~ pg3))
ggplot(autox) + geom_point(aes(horsepower, pg3[, 's(horsepower, 6)']),
                           color = "chocolate")
ggplot(autox) + geom_point(aes(acceleration, pg3[, 's(acceleration, 5)']),
                           color = "chocolate")
ggplot(autox) + geom_point(aes(weight, pg3[, 's(weight, 10)']),
                           color = "chocolate")
transf <- pg3[, 's(weight, 10)'] + pg3[, 's(acceleration, 5)'] + 
            pg3[, 's(horsepower, 6)']
ggplot(autox) + geom_point(aes(autox$mpg, transf), 
                           color = "chocolate")
var(pg3[, 's(horsepower, 6)'])
var(pg3[, 's(acceleration, 5)'])
var(pg3[, 's(weight, 10)'])

#Exercise 9
#Data = boston
rm(list = ls())
boston <- load.data(Boston)

## cubic polynomial regression
qplot(dis, nox, data = boston)
fit.d3 <- lm(nox ~ poly(dis, 3), data = boston)
tidy(fit.d3)
glance(fit.d3)

pred.fit.d3 <- predict(fit.d3, se = TRUE)
se <- 2 * pred.fit.d3$se.fit
se.bands <- cbind(pred.fit.d3$fit + se, pred.fit.d3$fit - se)
qplot(boston$dis, pred.fit.d3$fit)
ggplot(data = boston) +
    geom_point(aes(x = dis, y = nox), colour = "cornsilk4") +
    geom_line(aes(x = dis, y = pred.fit.d3$fit), colour = "purple3") +
    geom_ribbon(aes(x = dis, ymin = se.bands[, 2], ymax = se.bands[, 1]),
                fill = "ivory", alpha = 0.7) +
    labs(title = "Cubic Polynomial Regression")

## fit polynomials of different degrees
## return residual sum of squares for each
rss.poly <- function(deg) {
    poly.deg <- lm(nox ~ poly(dis, degree = deg), data = boston)
    resids.poly <- boston$nox - predict(poly.deg)
    return(sum(resids.poly^2))
}

rss_values <- unlist(map(1:15, rss.poly))
rss_values
qplot(1:15, rss_values)

# use cross validation to select optimal degree
cv.poly <- function(deg) {
    cv.glm(boston, 
           glm(nox ~ poly(dis, degree = deg), data = boston))$delta[1]
}

set.seed(1)
cv_mse_vals <- unlist(map(1:15, cv.poly))
qplot(1:15, cv_mse_vals[1:15])
which.min(cv_mse_vals)
cv_mse_vals[which.min(cv_mse_vals)]

qplot(dis, nox, data = boston)

## spline. knots selected via inspection of nox vs dis
spl.fit.1 <- lm(nox ~ bs(dis, knots = c(2.5, 7.5)), data = boston)
tidy(spl.fit.1)
glance(spl.fit.1)
pred.spl <- predict(spl.fit.1, se = TRUE)
se <- 2 * pred.spl$se
se.bands <- cbind(pred.spl$fit + se, pred.spl$fit - se)
ggplot(data = boston) + 
    geom_point(aes(x = dis, y = nox), color = "cornsilk4") +
    geom_line(aes(x = dis, y = pred.spl$fit), color = "gold4") +
    geom_ribbon(aes(x = dis, 
                    ymin = se.bands[, 2], 
                    ymax = se.bands[, 1]),
                fill = "ivory", alpha = 0.6) +
    labs(title = "Splines with specified knots (2.5, 7.5)")

spl.fit.2 <- lm(nox ~ bs(dis, df = 5), data = boston)
tidy(spl.fit.2)
glance(spl.fit.2)
attr(bs(boston$dis, df = 5),"knots")

pred.spl <- predict(spl.fit.2, se = TRUE)
se <- 2 * pred.spl$se
se.bands <- cbind(pred.spl$fit + se, pred.spl$fit - se)
ggplot(data = boston) + 
    geom_point(aes(x = dis, y = nox), color = "cornsilk4") +
    geom_line(aes(x = dis, y = pred.spl$fit), color = "gold4") +
    geom_ribbon(aes(x = dis, 
                    ymin = se.bands[, 2], 
                    ymax = se.bands[, 1]),
                fill = "ivory", alpha = 0.6) +
    labs(title = "Splines with df = 5")


spl.fit.3 <- lm(nox ~ bs(dis, df = 4), data = boston)
tidy(spl.fit.3)
glance(spl.fit.3)
attr(bs(boston$dis, df = 4),"knots")

pred.spl <- predict(spl.fit.3, se = TRUE)
se <- 2 * pred.spl$se
se.bands <- cbind(pred.spl$fit + se, pred.spl$fit - se)
ggplot(data = boston) + 
    geom_point(aes(x = dis, y = nox), color = "cornsilk4") +
    geom_line(aes(x = dis, y = pred.spl$fit), color = "gold4") +
    geom_ribbon(aes(x = dis, 
                    ymin = se.bands[, 2], 
                    ymax = se.bands[, 1]),
                fill = "ivory", alpha = 0.6) +
    labs(title = "Splines with df = 4")


spl.plot <- function(pred.spl) {
    #se <- 2 * pred.spl$se
    #se.bands <- cbind(pred.spl$fit + se, pred.spl$fit - se)
    ggplot(data = boston) + 
        geom_point(aes(x = dis, y = nox), color = "cornsilk4") +
        geom_line(aes(x = dis, y = pred.spl), color = "dodgerblue4") 
    #+
    #    geom_ribbon(aes(x = dis, 
    #                    ymin = se.bands[, 2], 
    #                    ymax = se.bands[, 1]),
    #                fill = "ivory", alpha = 0.6)
}

spl.df <- function(df) return(
    lm(nox ~ bs(dis, df = df), data = boston))

# apply model with df 3 to 10 and generate array with predictions for each df
# (rows = observations, cols = predictions)
pred.all <- sapply(map(3:10, spl.df), predict)
# calculate residuals and square them
resids.all <- boston$nox - pred.all
resids.sq.all <- resids.all^2
# all up the squared residuals in each column (= df)
rss_all <- apply(resids.sq.all, 2, sum)
qplot(3:10, rss_all)

map(1:8, function(df) spl.plot(pred.all[, df]))

### using cross-validation

spl.cv.df <- function(df) {
    cv.glm(boston, glm(nox ~ bs(dis, df = df), data = boston), K = 10)$delta[1]
}

set.seed(1)
cv.mse.spl.vals <- unlist(map(3:10, spl.cv.df))
qplot(3:10, cv.mse.spl.vals)
which.min(cv.mse.spl.vals) + 2

### Exerise 10
rm(list = ls())
source("subsets-plots.R")

load.data <- function(data) {
    data <- as.tibble(data)
    head(data)
    glimpse(data)
    return(data)
}
college <- load.data(College)
names(college) <- tolower(names(college))

set.seed(1)
train <- sample(c(TRUE, FALSE), nrow(college), replace = TRUE)
test <- !train
table(train, test)

regfit.best <- regsubsets(outstate ~., 
                          data = college[train,],
                          method = "forward",
                          nvmax = 17)
summary.sset.fit <- summary(regfit.best)
subsets_plots(summary.sset.fit, 17)
which.min(summary.sset.fit$bic)

# create design matrix
test.mat <- model.matrix(outstate ~., data = college[test,])

calc_rss <- function(i) {
    # extract coefficients
    coefi <- coef(regfit.best, id = i)
    # multiply design matrix by coefficients to get predictions
    pred <- test.mat[, names(coefi)] %*% coefi
    # square the residuals and take square root to get MSE
    return(mean((college$outstate[test] - pred)^2))
}

test_mse_vals <- sapply(1:17, calc_rss)
which.min(test_mse_vals)
test_mse_vals[which.min(test_mse_vals)]

# Using GAM with six (from BIC). All subset methods are returning 17 on
# validation
coef(regfit.best, 6)

college6 <- college %>% 
    select("outstate",
           "private",
           "room.board",
           "terminal",
           "perc.alumni", 
           "expend", 
           "grad.rate")

qplot(room.board, outstate, data = college6[train,])
qplot(room.board, outstate, color = private, data = college6[train,])

qplot(terminal, outstate, data = college6[train,])
qplot(terminal, outstate, color = private, data = college6[train,])

qplot(perc.alumni, outstate, data = college6[train,])
qplot(perc.alumni, outstate, color = private, data = college6[train,])

qplot(expend, outstate, data = college6[train,])
qplot(expend, outstate, color = private, data = college6[train,])

qplot(grad.rate, outstate, data = college6[train,])
qplot(grad.rate, outstate, color = private, data = college6[train,])

cor(college6[train,-2])
gam.1 <- gam(outstate ~ s(expend, 2) + private, data = college6[train,])
par(mfrow = c(1, 2))
plot.Gam(gam.1, se = TRUE, col = "dodgerblue")

gam.2 <- gam(outstate ~ s(expend, 3) + 
                 s(room.board, 1) +
                 private, data = college6[train,])
par(mfrow = c(1, 3))
plot.Gam(gam.2, se = TRUE, col = "dodgerblue")

gam.3 <- gam(outstate ~ s(expend, 3) + 
                 s(room.board, 1) + 
                 s(grad.rate, 2) +
                 private, data = college6[train,])
par(mfrow = c(2, 2))
plot.Gam(gam.3, se = TRUE, col = "dodgerblue")

gam.4 <- gam(outstate ~ s(expend, 3) + 
                 s(room.board, 1) + 
                 s(grad.rate, 2) +
                 s(perc.alumni, 2) +
                 private, data = college6[train,])
par(mfrow = c(2, 3))
plot.Gam(gam.4, se = TRUE, col = "dodgerblue")

gam.5 <- gam(outstate ~ s(expend, 3) + 
                 s(room.board, 1) + 
                 s(grad.rate, 2) +
                 s(perc.alumni, 2) +
                 s(terminal, 1) +
                 private, data = college6[train,])
par(mfrow = c(2, 3))
plot.Gam(gam.5, se = TRUE, col = "dodgerblue")
anova(gam.1, gam.2, gam.3, gam.4, gam.5, test = "F")

# From ANOVA it seems that adding terminal may not be useful
# Use gam.4
summary(gam.4)

# predictions on training set
preds.gam = predict(gam.4, newdata = college[test,])
resid.gam = college$outstate[test] - preds.gam
# residuals against fitted
qplot(preds.gam, resid.gam)

# fitting on full data
gam.full <- gam(outstate ~ s(expend, 3) + 
                 s(room.board, 1) + 
                 s(grad.rate, 2) +
                 s(perc.alumni, 2) +
                 private, data = college6)
par(mfrow = c(2, 3))
plot.Gam(gam.full, se = TRUE, col = "chocolate")

preds.gam = predict(gam.full, se = TRUE)
resid.gam = college6$outstate - preds.gam$fit
# residuals against fitted
qplot(preds.gam$fit, resid.gam)


## examining partial terms in the college gam model
pg4 <- predict(gam.full, type = "terms")
summary(lm(college6$outstate ~ pg4))
ggplot(college6) + geom_point(aes(expend, pg4[, 's(expend, 3)']),
                           color = "chocolate")
ggplot(college6) + geom_point(aes(room.board, pg4[, 's(room.board, 1)']),
                           color = "chocolate")
ggplot(college6) + geom_point(aes(grad.rate, pg4[, 's(grad.rate, 2)']),
                           color = "chocolate")
ggplot(college6) + geom_point(aes(perc.alumni, pg4[, 's(perc.alumni, 2)']),
                              color = "chocolate")

transf <- pg4[, 's(expend, 3)'] + pg4[, 's(room.board, 1)'] + 
    pg4[, 's(grad.rate, 2)'] + pg4[, 's(perc.alumni, 2)']
ggplot(college6) + geom_point(aes(outstate, transf), 
                           color = "dodgerblue4")

var(pg4[, 's(expend, 3)'])
var(pg4[, 's(room.board, 1)'])
var(pg4[, 's(grad.rate, 2)'])
var(pg4[, 's(perc.alumni, 2)'])

# Exercise 11
rm(list = ls())
b0 <- -0.5
b1 <- 2.0
b2 <- 1.2

set.seed(1)
x1 <- rnorm(100)
x2 <- rnorm(100)
y <- b0 + b1 * x1 + b2 * x2 + rnorm(100)

#a <-  y - b1 * x1 # what is left over after accounting for x1
#b2 <- lm(a ~ x2)$coef[2] # how much of that can be explained by x2

#a <- y - b2 * x2 # what is left over after accounting for x2
#b1 <- lm(a ~ x1)$coef[2] # how much of that can be explained by x1
b1 <- 50.0
vals <- rep(NA, 30)
dim(vals) <- c(10, 3)

for (i in 1:10) {
    a <-  y - b1 * x1
    b2 <- lm(a ~ x2)$coef[2]
    print(b2)
    
    #b0 <- lm(a ~ x2)$coef[1]
    #print(b0)
    
    a <- y - b2 * x2
    b1 <- lm(a ~ x1)$coef[2]
    print(b1)
    
    b0 <- lm(a ~ x1)$coef[1]
    print(b0)
    print("---------")
    
    vals[i, 1] <- b0
    vals[i, 2] <- b1
    vals[i, 3] <- b2
}
