setwd("/Users/anggunberlian/Desktop/CS112")
library(foreign)
library(Matching)
mydata <- read.dta("dw_data.dta")
head(mydata)
glm1 <- glm(treat~age + I(age^2) + education + I(education^2) + black +
              
              hispanic + married + nodegree + re74 + I(re74^2) + re75 + I(re75^2), family=binomial, data=mydata)



#save data objects

X <- glm1$fitted

Y <- mydata$re78

Tr <- mydata$treat



# one-to-one matching with replacement (the "M=1" option).

# Estimating the treatment effect on the treated (the "estimand" option defaults to ATT).


rr <- Match(Y=Y, Tr=Tr, X=X, M=1)
summary(rr)

# Let's check the covariate balance

# 'nboots' is set to small values in the interest of speed.

# Please increase to at least 500 each for publication quality p-values.

mb <- MatchBalance(treat~age + I(age^2) + education + I(education^2) + black +
                     
                     hispanic + married + nodegree + re74 + I(re74^2) + re75 + I(re75^2), data=mydata, match.out=rr, nboots=500)


#M2

rr2 <- Match(Y=Y, Tr=Tr, X=X, M=2)

mb <- MatchBalance(treat~age + I(age^2) + education + I(education^2) + black +
                     
                     hispanic + married + nodegree + re74 + I(re74^2) + re75 + I(re75^2), data=mydata, match.out=rr2, nboots=500)


## GENETIC MATCHING

