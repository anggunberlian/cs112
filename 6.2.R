setwd("/Users/anggunberlian/Desktop/CS112")
new_lalonde <- read.csv("observational_lalonde - observational_lalonde.csv")
head(new_lalonde)
reg <- lm(re78 ~ treat, data = new_lalonde)
reg1 <- lm(re78 ~ treat, age, educ, data = new_lalonde)

reg1
