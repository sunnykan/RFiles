library("tidyverse")
library("ISLR")

advertising <- read_csv("data/Advertising.csv")
names(advertising)[1:2] <- c("markets", "tv")

#Applied (Page 55)
#Exercise 8c
college <- as.tibble(College)
names(college) <- tolower(names(college))
glimpse(college)
summary(college)
# pairs(college[1:10])
ggplot(data = college, aes(x = private, y = outstate)) +
    geom_boxplot()

#create new variable
college %>% 
    mutate(elite = as_factor(if_else(top10perc > 50.0, "Yes", "No"))) -> college
summary(college$elite)

ggplot(data = college, aes(x = elite, y = outstate)) +
    geom_boxplot()
