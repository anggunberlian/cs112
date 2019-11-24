setwd("/Users/anggunberlian/Desktop/CS112")
## THE SET UP

library(Matching)
foo <- read.csv("peace.csv")

################
#  QUESTION 2  #
################
# Replicate figure 8 in https://gking.harvard.edu/files/counterf.pdf
# EXCEPT instead of the original interaction term, add two other interaction terms: (exp*untype4) and (wardur*logcost)

# extract relevant columns
foo2 <- foo[, c(6:8, 11:16, 99, 50, 114, 49, 52, 63, 136, 109, 126, 48, 160, 142, 10, 108)]

# remove 2 rows with missing data (there are better ways to handle missing data)
foo2 <- foo2[c(-19, -47), ]

# check that all missing data is gone...
which(is.na(foo2) == TRUE)

# take a peek at the data set (identify the columns)
head(foo2)

# Train the two models (without and with interaction term)
glm1 <- glm(pbs2s3 ~ wartype + logcost + wardur + factnum + factnum2 + 
              trnsfcap + develop + exp + decade + treaty + untype4, 
            data = foo2, family = binomial)
summary(glm1)

glm.int <- glm(pbs2s3 ~ wartype + logcost + wardur + factnum + factnum2 + 
                     trnsfcap + develop + exp + decade + treaty + untype4 + exp*untype4 + wardur*logcost, 
                   data = foo2, family = binomial)
summary(glm.int)

# Predictors at their means
mean.wartype <- mean(foo2$wartype)
mean.logcost <- mean(foo2$logcost)
mean.logdead <- mean(foo2$logcost)
mean.factnum <- mean(foo2$factnum)
mean.factnum2 <- mean(foo2$factnum2)
mean.trnsfcap <- mean(foo2$trnsfcap)
mean.develop <- mean(foo2$develop)
mean.exp <- mean(foo2$exp)
mean.decade <- mean(foo2$decade)
mean.treaty <- mean(foo2$treaty)

# Get logit auxiliary function
get_logit <- function(X, coef) {
  logit <- coef[1] + sum(coef[2:length(coef)]*X)
  return(exp(logit) / (1 + exp(logit)))
}

storage.original.treat <- rep(NA, 315)
storage.original.control <- rep(NA, 315)

# For each war duration
for (wardur in 1:315) {
  
  # Hypothetical unit with predictors held at their means, for both
  # treatment and control group, and varying wardur.
  X.treat <- c(mean.wartype, mean.logcost, wardur, mean.factnum, mean.factnum2, 
               mean.trnsfcap, mean.develop, mean.exp, mean.decade, mean.treaty, 1)
  X.control <- c(mean.wartype, mean.logcost, wardur, mean.factnum, mean.factnum2, 
                 mean.trnsfcap, mean.develop, mean.exp, mean.decade, mean.treaty, 0)
  
  storage.original.treat[wardur]  <- get_logit(X.treat, coef(glm1))
  storage.original.control[wardur]  <- get_logit(X.control, coef(glm1))
}

# Marginal treatment effect is y_treat - y_control
original_y <- storage.original.treat - storage.original.control

# Same process, but for interaction model
storage.int.treat <- rep(NA, 315)
storage.int.control <- rep(NA, 315)
for (wardur in 1:315) {
  X.treat <- c(mean.wartype, mean.logdead, wardur, mean.factnum, mean.factnum2, 
               mean.trnsfcap, mean.develop, mean.exp, mean.decade, mean.treaty, 1, mean.exp*1, wardur*mean.logcost)
  X.control <- c(mean.wartype, mean.logdead, wardur, mean.factnum, mean.factnum2, 
                 mean.trnsfcap, mean.develop, mean.exp, mean.decade, mean.treaty, 0, 0, wardur*mean.logcost)
  storage.int.treat[wardur]  <- get_logit(X.treat, coef(glm.int))
  storage.int.control[wardur]  <- get_logit(X.control, coef(glm.int))
}
int_y <- storage.int.treat - storage.int.control

# Plot

plot(1:315, original_y, type = "l", ylim = c(0, 0.8), xlim = c(1, 315), xlab = "War Duration in Months", 
     ylab = "Marginal effects of UN peacekeeping operations", pch=18)
lines(1:315, int_y, col = "red", ylim = c(0, 0.8), xlim = c(1, 315), pch=19, lty=2)
legend("topright",
       c("Original Model","Model with Interaction Terms"),
       fill=c("black","red")
)

################
#  QUESTION 3  #
################
#Tr <- rep(0, length(foo$uncint))
#Tr[which(foo$uncint != "None")] <- 1
#Tr


################
#  QUESTION 4  #
################
foo <- read.csv("peace.csv")

# extract relevant columns
foo4 <- foo[, c(6:8, 11:16, 34:35, 99, 50, 52, 114, 49, 63, 136, 109, 126, 48, 160, 142, 10)]

# remove rows with missing data
foo4 <- foo4[c(-4, -16, -19, -47, -84, -93, -98), ]

# check that all missing data is gone...
which(is.na(foo4) == TRUE)

# take a peek at the data set (identify the columns)
head(foo4)

# simple regression
# change pbs2l and pbs5l into binary
foo4$pbs2l <- ifelse(foo4$pbs2l == 'Success', 1,0)
foo4$pbs5l <- ifelse(foo4$pbs5l == 'Success', 1,0)

Tr <- rep(0, length(foo4$uncint))
Tr[which(foo4$uncint != "None" & foo4$uncint != "Mediation")] <- 1
foo4$Tr <- Tr

# Balance before matching (for the logistic regression model)
mb1 <- MatchBalance(Tr ~ wartype + logcost + wardur + factnum + factnum2 + 
                      trnsfcap + develop + exp + decade + treaty, data = foo4, nboots=500)

# Two years
glm3 <- glm(pbs2l ~ wartype + logcost + wardur + factnum + factnum2 + 
              trnsfcap + develop + exp + decade + treaty + Tr, 
            data = foo4, family = binomial)
summary(glm3)

# Five years
glm4 <- glm(pbs5l ~ wartype + logcost + wardur + factnum + factnum2 + 
              trnsfcap + develop + exp + decade + treaty + Tr, 
            data = foo4, family = binomial)
summary(glm4)

# Two years
foo.counter_factual <- foo4
foo.counter_factual$Tr <- rep(1, nrow(foo4)) - foo4$Tr

counter.factuals <- predict(glm3, newdata=foo.counter_factual, type="response")
fitted.values <- predict(glm3, newdata = foo4, type = "response")
unit_treat_effects <- rep(NA, nrow(foo4))

mask <- foo4$Tr == 1
unit_treat_effects[mask] <- glm3$fitted.values[mask] - counter.factuals[mask]
unit_treat_effects[!mask] <- counter.factuals[!mask] - glm3$fitted.values[!mask]
mean(unit_treat_effects)
sd(unit_treat_effects)

# Five years
foo.counter_factual <- foo4
foo.counter_factual$Tr <- 1 - foo4$Tr
counter.factuals <- predict(glm4, newdata=foo.counter_factual, type="response")
unit_treat_effects <- rep(NA, nrow(foo4))

mask <- foo4$Tr == 1
unit_treat_effects[mask] <- glm4$fitted.values[mask] - counter.factuals[mask]
unit_treat_effects[!mask] <- counter.factuals[!mask] - glm4$fitted.values[!mask]
mean(unit_treat_effects)
sd(unit_treat_effects)

### Propensity Score Matching

glm5 <- glm(Tr ~ wartype + logcost + wardur + factnum + factnum2 + 
              trnsfcap + develop + exp + decade + treaty, data = foo4, family = binomial)
summary(glm5)

X <- glm5$fitted.values
Y1 <- foo4$pbs2l
Y2 <- foo4$pbs5l
m1  <- Match(Y=Y1, Tr=Tr, X=X, M=1)
summary(m1)

mb2 <- MatchBalance(Tr ~ wartype + logcost + wardur + factnum + factnum2 + 
                      trnsfcap + develop + exp + decade + treaty, data = foo4, 
                    match.out = m1, nboots=500)


mask <- which(!is.na(Y2))
m2  <- Match(Y=Y2[mask], Tr=Tr[mask], X=X[mask], M=1)
summary(m2)

mb3 <- MatchBalance(Tr ~ wartype + logcost + wardur + factnum + factnum2 + 
                      trnsfcap + develop + exp + decade + treaty, data = foo4, 
                    match.out = m2, nboots=500)


### Genetic Matching

X = cbind(foo4$wartype, foo4$logcost, foo4$wardur, foo4$factnum, 
          foo4$factnum2, foo4$trnsfcap, foo4$develop, foo4$exp, foo4$decade, foo4$treaty)

Y1 <- foo4$pbs2l
Y2 <- foo4$pbs5l
Tr <- foo4$Tr

genout <- GenMatch(Tr=Tr, X=X, M=1,
                   pop.size=200, max.generations=10, wait.generations=25)
m3  <- Match(Y=Y1, Tr=Tr, X=X, M=1, Weight.matrix = genout)
summary(m3)

mb4 <- MatchBalance(Tr ~ wartype + logcost + wardur + factnum + factnum2 + 
                      trnsfcap + develop + exp + decade + treaty, data = foo4, 
                    match.out = m3, nboots=1000)

genout1 <- GenMatch(Tr=Tr[mask], X=X[mask,], M=1,
                    pop.size=200, max.generations=10, wait.generations=25)
m4  <- Match(Y=Y2[mask], Tr=Tr[mask], X=X[mask,], M=1, Weight.matrix = genout1)
summary(m4)

mb5 <- MatchBalance(Tr ~ wartype + logcost + wardur + factnum + factnum2 + 
                      trnsfcap + develop + exp + decade + treaty, data = foo4, 
                    match.out = m4, nboots=1000)


### Genetic Matching w/ Propensity Score

Xp = cbind(glm5$fitted.values, foo4$wartype, foo4$logcost, foo4$wardur, foo4$factnum, 
          foo4$factnum2, foo4$trnsfcap, foo4$develop, foo4$exp, foo4$decade, foo4$treaty)

Y1 <- foo4$pbs2l
Y2 <- foo4$pbs5l
Tr <- foo4$Tr

genout <- GenMatch(Tr=Tr, X=Xp, M=1,
                   pop.size=200, max.generations=10, wait.generations=25)
m6  <- Match(Y=Y1, Tr=Tr, X=Xp, M=1, Weight.matrix = genout)
summary(m6)

mb7 <- MatchBalance(Tr ~ wartype + logcost + wardur + factnum + factnum2 + 
                      trnsfcap + develop + exp + decade + treaty, data = foo4, 
                    match.out = m6, nboots=1000)

genout1 <- GenMatch(Tr=Tr[mask], X=Xp[mask,], M=1,
                    pop.size=200, max.generations=10, wait.generations=25)
m8  <- Match(Y=Y2[mask], Tr=Tr[mask], X=X[mask,], M=1, Weight.matrix = genout1)
summary(m8)

mb9 <- MatchBalance(Tr ~ wartype + logcost + wardur + factnum + factnum2 + 
                      trnsfcap + develop + exp + decade + treaty, data = foo4, 
                    match.out = m8, nboots=1000)


