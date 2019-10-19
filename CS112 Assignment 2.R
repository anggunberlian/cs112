setwd("/Users/anggunberlian/Desktop/CS112")
library(boot)
### QUESTION 1 ###

#1a Generating data where processed meat impact health in a negative way

processed_meat <- rnorm(998, mean = 150, sd = 65)
noise <- rnorm(998, mean = 0, sd = 120)
healthy <- (noise - processed_meat) / 100
healthy_meat_relationship <- data.frame(healthy, processed_meat)
reg <- lm(healthy ~ processed_meat)

# Plotting 998

plot(processed_meat, healthy, xlab = "Processed Meat Per Day (In Grams)", ylab = "Health Level", 
     ylim = c(-10, 15), xlim = c(0, 2000))
lines(processed_meat, reg$fitted.values, col = "red")
summary(reg)

#1b Adding two outliers

outliers <- data.frame(c(19, 15), c(2000, 1500))
names(outliers) <- c("healthy", "processed_meat")
health_relationship_outliers <- rbind(healthy_meat_relationship, outliers)
reg2 <- lm(healthy ~ processed_meat, data = health_relationship_outliers)

# Plotting 1000

plot(health_relationship_outliers$processed_meat, health_relationship_outliers$healthy, 
     xlab = "Processed Meat Per Day (In Grams)", ylab = "Health Level", 
     ylim = c(-10, 15), xlim = c(0, 2000))
lines(health_relationship_outliers$processed_meat,reg2$fitted.values, col = "blue")
summary(reg2)

# Plotting together
plot(health_relationship_outliers$processed_meat, health_relationship_outliers$healthy, 
     xlab = "Processed Meat Per Day (In Grams)", ylab = "Health Level", 
     ylim = c(-10, 15), xlim = c(0, 2000))
lines(processed_meat, reg$fitted.values, col = "red")
lines(health_relationship_outliers$processed_meat,reg2$fitted.values, col = "blue")


### QUESTION 2 ###

#2 Lalonde and re78 as a linear additive function of age, age^2, educ, treat, treat*age, re74, and re75

library(Matching)
data(lalonde)
age_square <- lalonde$age^2
new_lalonde <- cbind(lalonde, age_square)
reg_lalonde <- lm(re78 ~ age + age_square + educ + treat + treat*age + re74 + re75, data = new_lalonde)

# Isolate the TREATED units
treated_lalonde <- new_lalonde[ which(new_lalonde$treat == 1),]

# Simulate
iterations <- 10000
sim.lalonde <- sim(reg_lalonde, n.sims = iterations)


### QUESTION 2 A ###

# Predict re78 for every TREATED unit of age for every set of coefficients holding at the means
simulated.ys_mean <- matrix(NA, nrow = iterations, ncol = length(
  min(treated_lalonde$age):max(treated_lalonde$age)))

mean.educ <- mean(treated_lalonde$educ)
mean.re74 <- mean(treated_lalonde$re74)
mean.re75 <- mean(treated_lalonde$re75)
for (age in min(treated_lalonde$age):max(treated_lalonde$age)) {
  Xs <- c(1, age, age^2, mean.educ, 1, mean.re74, mean.re75, 1*age)
  for (i in 1:iterations) {
    simulated.ys_mean[i, age + 1 - min(treated_lalonde$age)] <- sum(Xs*sim.lalonde@coef[i,])
  }
}
conf.intervals_mean <- apply(simulated.ys_mean, 2, quantile, probs = c(0.025, 0.975))
table_mean <- t(data.frame(conf.intervals_mean))
colnames(table_mean) <- c("Mean PI Lower Bound", "Mean PI Upper Bound")
table_mean <- data.frame(table_mean, mean.educ, mean.re74, mean.re75)
rownames(table_mean) <- min(treated_lalonde$age):max(treated_lalonde$age)
View(table_mean)

plot(x = c(1:100), y = c(1:100), type = "n", 
     xlim = c(min(treated_lalonde$age),max(treated_lalonde$age)), 
     ylim = c(-10000,20000), 
     main = "Re78 by Age With Predictors Held at The Means", xlab = "Age", 
     ylab = "Re78")

for (age in min(lalonde$age):max(lalonde$age)) {
  segments(
    x0 = age,
    y0 = conf.intervals_mean[1, age - min(treated_lalonde$age) + 1],
    x1 = age,
    y1 = conf.intervals_mean[2, age - min(treated_lalonde$age) + 1],
    lwd = 2)
}

### QUESTION 2B

# Predict re78 for every CONTROL unit of age for every set of coefficients holding at the means
control_lalonde <- new_lalonde[ which(new_lalonde$treat == 0),]

simulated.ys_mean_con <- matrix(NA, nrow = iterations, ncol = length(
  min(control_lalonde$age):max(control_lalonde$age)))

mean.educ_con <- mean(control_lalonde$educ)
mean.re74_con <- mean(control_lalonde$re74)
mean.re75_con <- mean(control_lalonde$re75)
for (age in min(lalonde$age):max(lalonde$age)) {
  Xs <- c(1, age, age^2, mean.educ_con, 0, mean.re74_con, mean.re75_con, 0*age)
  for (i in 1:iterations) {
    simulated.ys_mean_con[i, age + 1 - min(control_lalonde$age)] <- sum(Xs*sim.lalonde@coef[i,])
  }
}

conf.intervals_mean_con <- apply(simulated.ys_mean_con, 2, quantile, probs = c(0.025, 0.975))
table_mean_con <- t(data.frame(conf.intervals_mean_con))
colnames(table_mean_con) <- c("Mean PI Lower Bound", "Mean PI Upper Bound")
table_mean_con <- data.frame(table_mean_con, mean.educ, mean.re74, mean.re75)
rownames(table_mean_con) <- min(control_lalonde$age):max(control_lalonde$age)
View(table_mean_con)

plot(x = c(1:100), y = c(1:100), type = "n", 
     xlim = c(min(control_lalonde$age),max(control_lalonde$age)), 
     ylim = c(-10000,20000), 
     main = "Re78 by Age With Predictors Held at The Means (For Control Units)", xlab = "Age", 
     ylab = "Re78")

for (age in min(control_lalonde$age):max(control_lalonde$age)) {
  segments(
    x0 = age,
    y0 = conf.intervals_mean_con[1, age - min(control_lalonde$age) + 1],
    x1 = age,
    y1 = conf.intervals_mean_con[2, age - min(control_lalonde$age) + 1],
    lwd = 2)
}

### QUESTION 2C

# 95% interval of expected values for the treatment effect holding educ, re74, and re75 at their means

simulated.ys_mean_treatment <- matrix(NA, nrow = iterations, ncol = length(
  min(lalonde$age):max(lalonde$age)))

for (age in min(lalonde$age):max(lalonde$age)) {
  Xs.treat <- c(1, age, age^2, mean.educ, 1, mean.re74, mean.re75, 1*age)
  Xs.control <- c(1, age, age^2, mean.educ_con, 0, mean.re74_con, mean.re75_con, 0*age)
  for (i in 1:iterations) {
    simulated.ys_mean_treatment[i, age + 1 - min(lalonde$age)] <- sum(Xs.treat*sim.lalonde@coef[i,]) - sum(Xs.control*sim.lalonde@coef[i,])
  }
}

conf.intervals_treatment <- apply(simulated.ys_mean_treatment, 2, quantile, probs = c(0.025, 0.975))
table_mean_treatment <- t(data.frame(conf.intervals_treatment))
colnames(table_mean_treatment) <- c("Mean PI Lower Bound", "Mean PI Upper Bound")
table_mean_treatment <- data.frame(table_mean_treatment)
rownames(table_mean_treatment) <- min(lalonde$age):max(lalonde$age)
View(table_mean_treatment)

plot(x = c(1:100), y = c(1:100), type = "n", 
     xlim = c(min(lalonde$age),max(lalonde$age)), 
     ylim = c(-10000,20000), 
     main = "Expected values for the treatment effect", xlab = "Age", 
     ylab = "Re78")

for (age in min(lalonde$age):max(lalonde$age)) {
  segments(
    x0 = age,
    y0 = conf.intervals_treatment[1, age - min(lalonde$age) + 1],
    x1 = age,
    y1 = conf.intervals_treatment[2, age - min(lalonde$age) + 1],
    lwd = 2)
}

### QUESTION 2D

# 95% interval of expected values for the treatment effect holding educ, re74, and re75 at their medians

simulated.ys_median_treatment <- matrix(NA, nrow = iterations, ncol = length(
  min(lalonde$age):max(lalonde$age)))

med.educ <- median(treated_lalonde$educ)
med.re74 <- median(treated_lalonde$re74)
med.re75 <- median(treated_lalonde$re75)

med.educ_con <- median(control_lalonde$educ)
med.re74_con <- median(control_lalonde$re74)
med.re75_con <- median(control_lalonde$re75)

for (age in min(lalonde$age):max(lalonde$age)) {
  Xs.treat_med <- c(1, age, age^2, med.educ, 1, med.re74, med.re75, 1*age)
  Xs.control_med <- c(1, age, age^2, med.educ_con, 0, med.re74_con, med.re75_con, 0*age)
  for (i in 1:iterations) {
    simulated.ys_median_treatment[i, age + 1 - min(lalonde$age)] <- 
      sum(Xs.treat_med*sim.lalonde@coef[i,]) - sum(Xs.control_med*sim.lalonde@coef[i,] + rnorm(1, 0, sim.lalonde@sigma[i]))
  }
}

conf.intervals_treatment_med <- apply(simulated.ys_median_treatment, 2, quantile, probs = c(0.025, 0.975))
table_med_treatment <- t(data.frame(conf.intervals_treatment_med))
colnames(table_med_treatment) <- c("Mean PI Lower Bound", "Mean PI Upper Bound")
table_med_treatment <- data.frame(table_med_treatment)
rownames(table_med_treatment) <- min(lalonde$age):max(lalonde$age)
View(table_med_treatment)

plot(x = c(1:100), y = c(1:100), type = "n", 
     xlim = c(min(lalonde$age),max(lalonde$age)), 
     ylim = c(-100000,105000), 
     main = "The treatment effect holding some 
     variables at their medians", xlab = "Age", 
     ylab = "Re78")

for (age in min(lalonde$age):max(lalonde$age)) {
  segments(
    x0 = age,
    y0 = conf.intervals_treatment_med[1, age - min(lalonde$age) + 1],
    x1 = age,
    y1 = conf.intervals_treatment_med[2, age - min(lalonde$age) + 1],
    lwd = 2)
}

### Question 3

foo <- read.csv("afterschool-afterschool.csv")
reg_foo <- lm(MATH_SCORE ~ TREATMENT, data = foo)

# Declare bootstrapping function
iterations <- 10000
storage <- rep(NA, iterations)

for (i in 1:iterations) {
                   # sample space, # of samples, with replacement
  boot_idx = sample(1:nrow(foo), nrow(foo), replace = T)
  temp_lm = lm(MATH_SCORE ~ TREATMENT, data = foo[boot_idx,])
  storage[i] <- temp_lm$coefficients[2]
}

# Find simulated confidence intervals 
quantile(storage, c(0.025, 0.975))
lm.foo <- lm(MATH_SCORE ~ TREATMENT, data = foo)
confint(lm.foo)[2,]
data.frame(simulation = quantile(storage, c(0.025, 0.975)), analytical = confint(lm.foo)[2,])
hist(storage, main = "Bootstrapped Values for the Treatment Effect", 
     xlab = "Treatment Effect", ylab = "Count")


### Question 4

# Bootstrap function that takes Ys and predicted Ys as inputs, and outputs R2

# Create the R^2 function
foo.wo.na <- foo[!is.na(foo$MATH_SCORE), ]
rsquared <- function(ytrue, ypred) {
  rss <- sum((ytrue - ypred)**2)
  tss <- sum((ytrue - mean(ytrue))**2)
  return(1 - rss/tss)
}
lm.foo1 <- lm(MATH_SCORE ~ TREATMENT, data = foo.wo.na)
rsquared(foo.wo.na$MATH_SCORE, lm.foo1$fitted.values)
summary(lm.foo1)$r.sq

# Declare bootstrapping function
iterations_boot1 <- 20000
storage_r2 <- rep(NA, iterations)

for (i in 1:iterations_boot1) {
  boot_idx <- sample(1:nrow(foo.wo.na), nrow(foo.wo.na), replace = T)
  temp_lm <- lm(MATH_SCORE ~ TREATMENT, data = foo.wo.na[boot_idx,])
  fit_y <- temp_lm$fitted.values
  real_y <- foo.wo.na[boot_idx,]$MATH_SCORE
  storage_r2[i] <- rsquared(real_y, fit_y)
}


# Find confidence intervals for the r2
quantile(storage_r2, c(0.025, 0.975))

### ANOTHER WAY ###

# Declare bootstrapping function + find the R^2
iterations_boot2 <- 200000

r_squared2 <- function(true_y, predicted_y) {
  storage <- rep(NA, iterations_boot2)
  for (i in 1:iterations) {
    indices <- sample(1:length(true_y), length(true_y), replace = T)
    new_true_y <- true_y[indices]
    new_pred_y <- predicted_y[indices]
    rss <- sum((new_true_y - new_pred_y)**2)
    tss <- sum((new_true_y - mean(new_true_y))**2)
    storage[i] <- (1 - rss/tss)
  }
  return(storage)
}

storage_rr <- r_squared2(foo.wo.na$MATH_SCORE, lm.foo1$fitted.values)


# Find confidence intervals for the r2
quantile(storage_rr, c(0.025, 0.975))

### Question 5
foo_q <- read.csv(url("https://tinyurl.com/yx8tqf3k"))

set.seed(12345)
test_set_rows <- sample(1:length(foo_q$age), 2000, replace = FALSE)

foo.test_set <- foo_q[test_set_rows,]
foo.train_set <- foo_q[-test_set_rows,]

foo.train_set_rows <- sample(1:length(foo.train_set$age), 1000, replace = FALSE)
foo.train_set_here1 <- foo.train_set[foo.train_set_rows, ]


model_1 <- glm(treat ~ . - re78, data = foo.train_set_here1, family = binomial)
model_2 <- glm(treat ~ age + education + hispanic + re75 - re78, data = foo.train_set_here1, family = binomial)
model_3 <- glm(treat ~ age + education + hispanic + married - re78, data = foo.train_set_here1, family = binomial)
model_4 <- glm(treat ~ age + education + black + re74 + re75 - re78, data = foo.train_set_here1, family = binomial)
model_5 <- glm(treat ~ age + education + black + married - re78, data = foo.train_set_here1, family = binomial)


cv.err_1 <- cv.glm(foo.train_set_here1, model_1) 
cv.err_2 <- cv.glm(foo.train_set_here1, model_2)
cv.err_3 <- cv.glm(foo.train_set_here1, model_3)
cv.err_4 <- cv.glm(foo.train_set_here1, model_4)
cv.err_5 <- cv.glm(foo.train_set_here1, model_5)


# Test set error for the train set 

cv.err_1$delta
cv.err_2$delta
cv.err_3$delta
cv.err_4$delta
cv.err_5$delta

# Test set error for the test set

mean((foo.test_set$treat - predict(model_1, foo.test_set, type = "response"))^2)
mean((foo.test_set$treat - predict(model_2, foo.test_set, type = "response"))^2)
mean((foo.test_set$treat - predict(model_3, foo.test_set, type = "response"))^2)
mean((foo.test_set$treat - predict(model_4, foo.test_set, type = "response"))^2)
mean((foo.test_set$treat - predict(model_5, foo.test_set, type = "response"))^2)

# Classification Problem + Confusion Matrix

foo.train_set_rows1 <- sample(1:length(foo.train_set$age), 2000, replace = FALSE)
foo.train_set_here2 <- foo.train_set[foo.train_set_rows1, ]

model_1 <- glm(treat ~ . - re78, data = foo.train_set_here2, family = binomial)
predicted_probs.1 <- predict(model_1)
predicted_ys.1 <- rep(0, length(predicted_probs.1))
predicted_ys.1[predicted_probs.1 > 0.5] <- 1

table(predicted_ys.1, foo.train_set_here2$treat)

new.predicted_probs.1 <- predict.glm(model_1, foo.test_set, type = "response")
predicted_yn.1 <- rep(0, length(new.predicted_probs.1))
predicted_yn.1[new.predicted_probs.1 > 0.5] <- 1

table(predicted_yn.1, foo.test_set$treat)
