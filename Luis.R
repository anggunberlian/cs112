setwd("/Users/anggunberlian/Desktop/CS112")
luis <- data.frame(read.csv("CS112LuisSamples.csv"))

luis
reg <- lm(luis$CSS112.Score ~ luis$USD + luis$Age + luis$Political.View)
reg$fitted.values
errors <- luis$CSS112.Score - reg$fitted.values
se <- errors^2
sum_se <- sum(se)
mean <- mean(sum_se)
sqrt(mean)

luis2 <- data.frame(read.csv("CS112LuisSamples2.csv"))
reg2 <- lm(luis2$CSS112.Score ~ luis2$USD + luis2$Age + luis2$Political.View)
reg2$fitted.values
errors2 <- luis2$CSS112.Score - reg2$fitted.values
se2 <- errors2^2
sum_se_2 <- sum(se2)
mean2 <- mean(sum_se_2)
sqrt(mean2)
