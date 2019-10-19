setwd("/Users/anggunberlian/Desktop/CS112")
library(foreign)
df <- read.dta('turnout.dta')
df <- read_dta("C:/Users/Vinic/Downloads/turnout.dta")
View(df)
df[1,1]
df[1,]

lm2 <- glm(turnout ~ ., data = df, family = binomial)
summary(lm2)


get_turnout_prob <- function(coefs, person) {
  logit <- coefs[1] + person[1]*coefs[2] +
    person[2]*coefs[3] +
    person[3]*coefs[4] + 
    person[4]*coefs[5] +
    person[5]*coefs[6]
  
  return(exp(logit) / (1 + exp(logit)))
}

this_person <- c(0, 38, 12, 4, 14.44)
prob1 <- get_turnout_prob(lm2$coefficients, this_person)  # 74%

this_more_educated_person <- c(0, 38, 16, 4, 14.44)
prob2 <- get_turnout_prob(lm2$coefficients, this_more_educated_person)  # 86%

this_hypothetical_person <- c(mean(df$white), 38, 16, mean(df$income), 14.44)
prob3 <- get_turnout_prob(lm2$coefficients, this_hypothetical_person)  # 87%

sim.glm <- sim(lm2, 1000)

storage.vector <- rep(0, 1000)
for (i in 1:1000) {
  storage.vector[i] <- get_turnout_prob(sim.glm@coef[i, ], this_hypothetical_person)
}
quantile(storage.vector, probs = c(0.005, 0.995))

storage.matrix_undergrad <- matrix(NA, nrow = 1000, ncol = 78)
for (age in c(18:95)) {
  for (i in 1:1000)
  {
    undergrad_person <- c(mean(df$white), age, 16, mean(df$income), 0.01*age**2)
    storage.matrix_undergrad[i, age - 17] <- get_turnout_prob(sim.glm@coef[i, ], undergrad_person)
  }
}

storage.matrix_highschool <- matrix(NA, nrow = 1000, ncol = 78)
for (age in c(18:95)) {
  for (i in 1:1000)
  {
    highschool_person <- c(mean(df$white), age, 12, mean(df$income), 0.01*age**2)
    storage.matrix_highschool[i, age - 17] <- get_turnout_prob(sim.glm@coef[i, ], highschool_person)
  }
}

conf.intervals_undergrad <- apply(storage.matrix_undergrad, 2, quantile, probs = c(0.005, 0.995))
conf.intervals_highschool <- apply(storage.matrix_highschool, 2, quantile, probs = c(0.005, 0.995))

plot(x = c(1:100), y = c(1:100), type = "n", xlim = c(18,95), ylim = c(0,1), 
     main = "Probability of voting by age", xlab = "Age of Respondent", 
     ylab = "Probability of Voting")

for (age in 18:95) {
  segments(
    x0 = age,
    y0 = conf.intervals_undergrad[1, age - 17],
    x1 = age,
    y1 = conf.intervals_undergrad[2, age - 17],
    lwd = 2)
}
for (age in 18:95) {
  segments(
    x0 = age,
    y0 = conf.intervals_highschool[1, age - 17],
    x1 = age,
    y1 = conf.intervals_highschool[2, age - 17],
    lwd = 2)
}