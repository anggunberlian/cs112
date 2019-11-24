require(dplyr)
people_A <- c("a", "b", "c", "d", "e", "f", "j", "k")
people_B <- c("l", "m", "o", "p", "q", "r", "s", "t")

control <- c(0, 0, 0, 0, 0, 0, 0, 0)
treatment <- c(1, 1, 1, 1, 1, 1, 1, 1)

df_con <- data.frame(people_A, control) # I know it is not random this way :(
df_treat <- data.frame(people_B, treatment)

# I should then somehow 

observed.diffmeans <- mean(observed.turnout[c(2,4,6,8)]) - 
  mean(observed.turnout[c(1,3,5,7)])

print(observed.diffmeans)

foo <- data.frame(city.names, observed.turnout)

# Turnout Function
turnout <- function() {
  # Four coin flips, establishing random assignment
  assignment        <- foo[sample(1:2),]
  assignment[3:4,]  <- foo[sample(3:4),]
  assignment[5:6,]  <- foo[sample(5:6),]
  assignment[7:8,]  <- foo[sample(7:8),]
  
  treatment.group   <- assignment[c(1,3,5,7),]
  control.group     <- assignment[c(2,4,6,8),]
  
  return(mean(treatment.group[,2]) - mean(control.group[,2]))
}

# Iterating the Turnout Function
iter.RI <- function(iterations = 100000) {
  for (i in 1:iterations) 
  {storage.vector[i] <- turnout()
  }
  return(storage.vector)
}

storage.vector <- NULL
results <- iter.RI()

sum(unique(results))
# Exploring the results

quantile(results, prob = c(0.95, 0.975))

length(unique(results))

hist(results)
plot(density(results))
abline(v = 5, lwd = 2, col = "red")

yIf I have 16 villages.
I want to randomly select 8 of them.

How do I do that?
  
  
  
  
  treated.units <- sample(1:16, 8, replace = FALSE)
control.units <- 1:16[-treated.units]

diff.means <- mean(Y1s[treated.units]) - mean(Y0s[control.units])


