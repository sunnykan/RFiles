library("MASS")
library("ISLR")
library("tidyverse")
library("broom")
library("ggfortify")
library("car")

# generate diagnostic plots 
diag_plots <- function(fitted_model){
    autoplot(fitted_model, which = 1:6, colour = "dodgerblue3",
             smooth.colour = "red", smooth.linetype = "dashed",
             ad.colour = "blue",
             label.size = 3, label.n = 5, label.colour = "black",
             ncol = 2, alpha = 0.3)
}

advertising <- read_csv("./data/Advertising.csv")
glimpse(advertising)
summary(advertising)

ggplot(data = advertising, aes(x = TV, y = sales)) +
    geom_smooth(method = "lm", se = FALSE, color = "red", formula = y ~ x) +
    geom_point()

sales_lm <- lm(sales ~ TV, data = advertising)
summary(sales_lm)
tidy(sales_lm)
glance(sales_lm)

sales_full_lm <- lm(sales ~ TV + radio + newspaper, data = advertising)
tidy(sales_full_lm)
sales_full_lm_aug <- augment(sales_full_lm)
# RSquared = square of the correlation between response and predicted
cor(sales_full_lm_aug$sales, sales_full_lm_aug$.fitted) ** 2

sales_int_lm <- lm(sales ~ TV * radio, data = advertising)

# ----
boston <- as.tibble(Boston)
glimpse(boston)

lm.fit <- lm(medv ~ lstat, boston)
lm.fit
names(lm.fit)
coef(lm.fit)
confint(lm.fit)
predict(lm.fit, data.frame(lstat = c(5, 10, 15)), interval = "confidence")
predict(lm.fit, data.frame(lstat = c(5, 10, 15)), interval = "prediction")

ggplot(data = boston, aes(x = lstat, y = medv)) +
    geom_smooth(method = "lm", se = FALSE, color = "red") +
    geom_point()

lm.fit_aug <- augment(lm.fit)
ggplot(data = lm.fit_aug, aes(x = .fitted, y = .resid)) + 
    geom_point(color = "red", alpha = 0.5) + 
    geom_hline(yintercept = 0, color = "blue")

par(mfrow = c(2, 2))
plot(lm.fit)

diag_plots <- autoplot(lm.fit, which = 1:6, colour = "dodgerblue3",
         smooth.colour = "red", smooth.linetype = "dashed",
         ad.colour = "blue",
         label.size = 3, label.n = 5, label.colour = "black",
         ncol = 2, alpha = 0.3)

ggp <- ggplot(data = lm.fit_aug, aes(x = .fitted, y = .resid))
# residuals vs fitted
ggp + geom_point(alpha = 0.5) + 
    geom_smooth(color = "red", size = 0.5) + 
    geom_hline(yintercept = 0, color = "blue")

# studentized residuals vs fitted
# studentized = divide residuals by standard dev of sample
# standardizes residuals so they can be compared
ggp <- ggplot(data = lm.fit_aug, aes(x = .fitted, y = .std.resid))
ggp + geom_point(alpha = 0.5) + 
    geom_smooth(color = "red", size = 0.5) + 
    geom_hline(yintercept = 0, color = "blue")

#cook's distance
ggplot(data = lm.fit_aug, aes(x = c(1:506), y = .hat)) +
    geom_point(color = "darkgrey")
which.max(lm.fit_aug$.hat)

#----
lm_fit = lm(medv ~ lstat + age, data = boston)
summary(lm_fit)
tidy(lm_fit)
glance(lm_fit)
lm_fit_aug = augment(lm_fit)
lm_fit_aug

lm_fit_all = lm(medv ~., data = boston)
vif(lm_fit_all)
cor(boston)

#without age
tidy(lm(medv ~.-age, data = boston))
glance(lm(medv ~.-age, data = boston))

# without age using update function
update(lm_fit_all, ~.-age)

# interaction
lm_fit_int <- lm(medv ~ lstat*age, data = boston)
tidy(lm_fit_int)
glance(lm_fit_int)

# squared term
lm_fit_sq = lm(medv ~ lstat + I(lstat^2), data = boston)
tidy(lm_fit_sq)
glance(lm_fit_sq)

lm.fit = lm(medv ~ lstat)
anova(lm.fit, lm_fit_sq)

diag_plots <- autoplot(lm_fit_sq, which = 1:6, colour = "dodgerblue3",
                       smooth.colour = "red", smooth.linetype = "dashed",
                       ad.colour = "blue",
                       label.size = 3, label.n = 5, label.colour = "black",
                       ncol = 2, alpha = 0.3)
diag_plots

# fifth order polynomial fit using poly() function

lm_fit_poly5 = lm(medv ~ poly(lstat, 5), data = boston)
tidy(lm_fit_poly5)
glance(lm_fit_poly5)
diag_plots <- autoplot(lm_fit_poly5, which = 1:6, colour = "dodgerblue3",
                       smooth.colour = "red", smooth.linetype = "dashed",
                       ad.colour = "blue",
                       label.size = 3, label.n = 5, label.colour = "black",
                       ncol = 2, alpha = 0.3)
diag_plots

## Carseats

carseats <- as.tibble(Carseats)
names(carseats) <- tolower(names(carseats))

lm_carseats = lm(sales ~. + income:advertising + price:age, data = carseats)
summary(lm_carseats)
tidy(lm_carseats)
glance(lm_carseats)

contrasts(carseats$shelveloc)

# -----
# Exercises
# -----

# Question 8
auto <- as.tibble(Auto)
glimpse(auto)
lm_fit <- lm(mpg ~ horsepower, data = auto)
summary(lm_fit)
glance(lm_fit)
tidy(lm_fit)

predict(lm_fit, data.frame(horsepower = 98), interval = "confidence")
predict(lm_fit, data.frame(horsepower = 98), interval = "prediction")

ggplot(data = auto, aes(x = horsepower, y = mpg)) +
    geom_smooth(method = "lm", color = "red", se = FALSE, size = 0.3) + 
    geom_point(alpha = 0.3)

diag_plots <- autoplot(lm_fit, which = 1:6, colour = "dodgerblue3",
                       smooth.colour = "red", smooth.linetype = "dashed",
                       ad.colour = "blue",
                       label.size = 3, label.n = 5, label.colour = "black",
                       ncol = 2, alpha = 0.3)
diag_plots

# Question 9
pairs(auto)
cor(auto[1:8])
lm_fit_full <- lm(mpg ~.-name, data = auto) 
summary(lm_fit_full)
tidy(lm_fit_full)
glance(lm_fit_full)

diag_plots <- autoplot(lm_fit_full, which = 1:4, colour = "dodgerblue3",
                       smooth.colour = "red", smooth.linetype = "dashed",
                       ad.colour = "blue",
                       label.size = 3, label.n = 5, label.colour = "black",
                       ncol = 2, alpha = 0.3)
diag_plots

auto %>% 
    slice(-(14)) -> tmp
tmp <- slice(auto, -(14))
tmp_fit <- lm(mpg ~.-name, data = tmp) 
glance(tmp_fit)

hp2 <- lm(mpg ~ horsepower + I(horsepower^2), auto)
tidy(hp2)
glance(hp2)
hp2aug <- augment(hp2)
ggplot(hp2aug, aes(x = .fitted, y = .std.resid)) + 
    geom_point(alpha = 0.4) + geom_smooth(method = "lm", size = 0.3)

ggplot(hp2aug, aes(x = .fitted, y = .std.resid)) + 
    geom_point(alpha = 0.4) + geom_smooth(method = "loess", size = 0.3)

diag_plots <- autoplot(hp2, which = 1:6, colour = "dodgerblue3",
                       smooth.colour = "red", smooth.linetype = "dashed",
                       ad.colour = "blue",
                       label.size = 3, label.n = 5, label.colour = "black",
                       ncol = 2, alpha = 0.3)
diag_plots

hp2o <- lm(mpg ~ horsepower + I(horsepower^2) + origin, auto)
tidy(hp2o)
glance(hp2o)
summary(hp2o)
diag_plots <- autoplot(hp2o, which = 1, colour = "dodgerblue3",
                       smooth.colour = "red", smooth.linetype = "dashed",
                       ad.colour = "blue",
                       label.size = 3, label.n = 5, label.colour = "black",
                       ncol = 2, alpha = 0.3)

hp2oi <- lm(mpg ~ horsepower + I(horsepower^2) + 
                origin + horsepower:origin, auto)
glance(hp2oi)
diag_plots

auto %>% 
    mutate(l10hp = log10(horsepower)) -> l10_hp
summary(lm(mpg ~ l10hp, l10_hp))    
summary(lm(mpg ~ l10hp + I(l10hp^5), l10_hp))
p <- lm(mpg ~ poly(l10hp, 2), l10_hp)

diag_plots <- autoplot(p, which = 1, colour = "dodgerblue3",
                       smooth.colour = "red", smooth.linetype = "dashed",
                       ad.colour = "blue",
                       label.size = 3, label.n = 5, label.colour = "black",
                       ncol = 2, alpha = 0.3)
diag_plots

auto %>% 
    mutate(sqrthp = sqrt(horsepower)) -> sqrt_hp
summary(lm(mpg ~ sqrthp, sqrt_hp))
p <- lm(mpg ~ sqrthp + , sqrt_hp)
diag_plots <- autoplot(p, which = 1, colour = "dodgerblue3",
                       smooth.colour = "red", smooth.linetype = "dashed",
                       ad.colour = "blue",
                       label.size = 3, label.n = 5, label.colour = "black",
                       ncol = 2, alpha = 0.3)
diag_plots
#-------
# Example 10

m1 <- lm(sales ~ price + urban + us, data = carseats)
tidy(m1)
glance(m1)
summary(m1)

m2 <- lm(sales ~ price + us, data = carseats)
tidy(m2)
glance(m2)
summary(m2)

tidy(m2)["estimate"] + qt(.975, 397) * tidy(m2)["std.error"]
tidy(m2)["estimate"] - qt(.975, 397) * tidy(m2)["std.error"]

diag_plots <- autoplot(m2, which = 1:6, colour = "dodgerblue3",
                       smooth.colour = "red", smooth.linetype = "dashed",
                       ad.colour = "blue",
                       label.size = 3, label.n = 5, label.colour = "black",
                       ncol = 2, alpha = 0.3)
diag_plots

# Example 11
set.seed(1)
x = rnorm(100)
y = 2 * x + rnorm(100)
lm.fit_yx <- lm(y ~ x + 0)
tidy(lm.fit_yx)
lm.fit_xy <- lm(x ~ y + 0)
tidy(lm.fit_xy)

# Exercise 13
set.seed(1)
X = rnorm(100, 0, 1)
summary(X)
eps = rnorm(100, 0, sqrt(0.25))
summary(eps)
Y = -1 + 0.5*X + eps
lm.fit = lm(Y ~ X)
tidy(lm.fit)
glance(lm.fit)
summary(lm.fit)

ggplot(data = data.frame(X, Y), aes(x = X, y = Y)) + 
    geom_smooth(method = "lm", color = "red", se = FALSE) + 
    geom_point() + 
    geom_abline(slope = 0.5, intercept = -1, color = "green")

diag_plots <- autoplot(lm.fit, which = 1, colour = "dodgerblue3",
                       smooth.colour = "red", smooth.linetype = "dashed",
                       ad.colour = "blue",
                       label.size = 3, label.n = 5, label.colour = "black",
                       ncol = 2, alpha = 0.3)
diag_plots

lm.fit_poly2 = lm(Y ~ X + I(X^2))
summary(lm.fit_poly2)
diag_plots <- autoplot(lm.fit_poly2, which = 1, colour = "dodgerblue3",
                       smooth.colour = "red", smooth.linetype = "dashed",
                       ad.colour = "blue",
                       label.size = 3, label.n = 5, label.colour = "black",
                       ncol = 2, alpha = 0.3)
diag_plots

# LESS NOISE

eps = rnorm(100, mean = 0, sd = 0.10)
Y = -1 + 0.5*X + eps
lm.fith = lm(Y ~ X)
tidy(lm.fith)
glance(lm.fith)
summary(lm.fith)
ggplot(data = data.frame(X, Y), aes(x = X, y = Y)) + 
    geom_smooth(method = "lm", color = "red", se = FALSE) + 
    geom_point() + 
    geom_abline(slope = 0.5, intercept = 1, color = "green")

diag_plots <- autoplot(lm.fith, which = 1, colour = "dodgerblue3",
                       smooth.colour = "red", smooth.linetype = "dashed",
                       ad.colour = "blue",
                       label.size = 3, label.n = 5, label.colour = "black",
                       ncol = 2, alpha = 0.3)
diag_plots

lm.fith_poly2 = lm(Y ~ X + I(X^2))
tidy(lm.fith_poly2)
glance(lm.fith_poly2)
summary(lm.fith_poly2)

diag_plots <- autoplot(lm.fith_poly2, which = 1, colour = "dodgerblue3",
                       smooth.colour = "red", smooth.linetype = "dashed",
                       ad.colour = "blue",
                       label.size = 3, label.n = 5, label.colour = "black",
                       ncol = 2, alpha = 0.3)
diag_plots

# MORE NOISE
eps = rnorm(100, 0, 1.0)
Y = -1 + 0.5*X + eps
lm.fiti = lm(Y ~ X)
tidy(lm.fiti)
glance(lm.fiti)
summary(lm.fiti)

ggplot(data = data.frame(X, Y), aes(x = X, y = Y)) + 
    geom_smooth(method = "lm", color = "red", se = FALSE) + 
    geom_point() + 
    geom_abline(slope = 0.5, intercept = -1, color = "green")

diag_plots <- autoplot(lm.fith, which = 1, colour = "dodgerblue3",
                       smooth.colour = "red", smooth.linetype = "dashed",
                       ad.colour = "blue",
                       label.size = 3, label.n = 5, label.colour = "black",
                       ncol = 2, alpha = 0.3)
diag_plots

# POLY 2 fit
lm.fiti_poly2 = lm(Y ~ X + I(X^2))
tidy(lm.fiti_poly2)
glance(lm.fiti_poly2)
summary(lm.fiti_poly2)

diag_plots <- autoplot(lm.fiti_poly2, which = 1, colour = "dodgerblue3",
                       smooth.colour = "red", smooth.linetype = "dashed",
                       ad.colour = "blue",
                       label.size = 3, label.n = 5, label.colour = "black",
                       ncol = 2, alpha = 0.3)
diag_plots

# Example 14

set.seed(1)
x1 = runif(100)
x2 = 0.5 * x1 + rnorm(100)/10
y = 2 + 2 * x1 + 0.3 * x2 + rnorm(100)
cor(x1, x2)
ggplot(data = data.frame(x1, x2), aes(x = x1, y = x2)) +
    geom_point(color = "blue", alpha = 0.4)

lm.fit14 <- lm(y ~ x1 + x2)
tidy(lm.fit14)
glance(lm.fit14)
summary(lm.fit14)

lm.fit14x1 <- lm(y ~ x1)
tidy(lm.fit14x1)
glance(lm.fit14x1)
summary(lm.fit14x1)

lm.fit14x2 <- lm(y ~ x2)
tidy(lm.fit14x2)
glance(lm.fit14x2)
summary(lm.fit14x2)

# Repeat analysis with new points
x1 = c(x1, 0.1)
x2 = c(x2, 0.8)
y = c(y, 6)

cor(x1, x2)
ggplot(data = data.frame(x1, x2), aes(x = x1, y = x2)) +
    geom_point(color = "blue", alpha = 0.4)

lm.fit14 <- lm(y ~ x1 + x2)
tidy(lm.fit14)
glance(lm.fit14)
summary(lm.fit14)

diag_plots <- autoplot(lm.fit14, which = 1:6, colour = "dodgerblue3",
                       smooth.colour = "red", smooth.linetype = "dashed",
                       ad.colour = "blue",
                       label.size = 3, label.n = 5, label.colour = "black",
                       ncol = 2, alpha = 0.3)
diag_plots

lm.fit14x1 <- lm(y ~ x1)
tidy(lm.fit14x1)
glance(lm.fit14x1)
summary(lm.fit14x1)

diag_plots <- autoplot(lm.fit14x1, which = 1:6, colour = "dodgerblue3",
                       smooth.colour = "red", smooth.linetype = "dashed",
                       ad.colour = "blue",
                       label.size = 3, label.n = 5, label.colour = "black",
                       ncol = 2, alpha = 0.3)
diag_plots

lm.fit14x2 <- lm(y ~ x2)
tidy(lm.fit14x2)
glance(lm.fit14x2)
summary(lm.fit14x2)

diag_plots <- autoplot(lm.fit14x2, which = 1:6, colour = "dodgerblue3",
                       smooth.colour = "red", smooth.linetype = "dashed",
                       ad.colour = "blue",
                       label.size = 3, label.n = 5, label.colour = "black",
                       ncol = 2, alpha = 0.3)
diag_plots

# EXERCISE 15

glimpse(boston)
features <- names(boston[2:14])
# See https://stackoverflow.com/questions/47609556/using-column-name-of-dataframe-as-predictor-variable-in-linear-regression

#generate a separate model for the regression of each feature on target (crim)
models = map(features, ~ lm(reformulate(.x, "crim"), data=boston))
#label each model with corresponding formula
names(models) = map(models, ~ .x[["terms"]])
#assess model performance
model_coefs = map_df(models, tidy, .id = "Model")
model_performance = map_df(models, glance, .id = "Model")
#print all rows
model_coefs %>% print(n = nrow(.))

#fit the full model (all variables)
model_full <- lm(crim ~., data = boston)
tidy(model_full)
glance(model_full)
full_coef_vals = tidy(model_full)[2:14, c("term", "estimate")]

#model_coefs[!model_coefs$term == "(Intercept)",]
#model_coefs[which(model_coefs$term != '(Intercept)'),]
uni_coef_vals = subset(model_coefs, term != "(Intercept)")[1:13, c("term", "estimate")]

coef_table <- uni_coef_vals %>% inner_join(full_coef_vals, by = "term")

ggplot(data = coef_table, aes(x = estimate.x, y = estimate.y)) +
    geom_point() + geom_text(aes(label = term),hjust = 0, vjust = 1) +
    xlab("Estimate from simple linear regression") +
    ylab("Estimate form multiple linear regression ")
coef_table %>% 
    filter(term != "nox") %>% 
    ggplot(aes(x = estimate.x, y = estimate.y)) + 
    geom_point(alpha = 0.3) +
    geom_text(aes(label = term),hjust = 1, vjust = 1) +
    geom_abline(slope = 1, intercept = 0, color = "orange") +
    xlab("Estimate from simple linear regression (w/o nox)") +
    ylab("Estimate form multiple linear regression (w/o nox")

#Fit polynomials of degree 3 to features
#Remove factor variable (chas)
features <- names(boston[-c(1,4)])
models_poly = map(features, 
                  ~ lm(reformulate(paste0("poly(", .x, ", ", 3, ")"), "crim"), data = boston))
model_coefs_poly = map_df(models_poly, tidy, .id = "Model")
model_performance_poly = map_df(models_poly, glance, .id = "Model")
model_coefs_poly %>% print(n = nrow(.))


