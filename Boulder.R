setwd("/Users/anggunberlian/Desktop/CS112")

total_boulder_days <- runif(200, min = 1, max = 75)
boulder_hours_per_day <- runif(200, min = 0.5, max = 4)
treatment_chalk <- sample(c(0, 1), 200, replace = TRUE)
noise <- rnorm(200, mean = 0, sd = 0.6)
v_level <- total_boulder_days + 1.5 * boulder_hours_per_day + treatment_chalk + noise
v_level
boulder_klub <- data.frame(total_boulder_days, boulder_hours_per_day, treatment_chalk, v_level)
boulder_klub
rnd_indices <- sample(nrow(boulder_klub), 100) # run once
slice_a <- boulder_klub[rnd_indices, ] # data you slice
slice_b <- boulder_klub[-rnd_indices, ] # data left
write.csv(boulder_klub[rnd_indices, ], "firstslide.csv")
write.csv(boulder_klub[-rnd_indices, ], "secondslide.csv")

plot(slice_a$total_boulder_days, slice_a$v_level, ylab="V level", xlab="Total boulder days", main="Slice A data")
plot(slice_b$total_boulder_days, slice_b$v_level, ylab="V level", xlab="Total boulder days", main="Slice B data")

v_level_true <- total_boulder_days + 1.5 * boulder_hours_per_day + treatment_chalk
boulder_klub_true <- data.frame(total_boulder_days, boulder_hours_per_day, treatment_chalk, v_level)
head(boulder_klub_true)
reg <- lm(v_level_true ~ total_boulder_days + boulder_hours_per_day + treatment_chalk)
reg$fitted.values
errors <- v_level_true - reg$fitted.values
se <- errors^2
sum_se <- sum(se)
mean <- mean(sum_se)
sqrt(mean) #RMSE

#R-SQUARED!!!
pred_boulder <- v_level
actual_boulder <- v_level_true
rss <- sum((pred_boulder - actual_boulder) ^ 2)
tss <- sum((actual_boulder - mean(actual_boulder)) ^ 2) 
rqs <- 1 - rss/tss
rqs
#write.csv(dataset, "filename.csv")
# r sample dataframe; selecting a random subset in r
# df is a data frame; pick 5 rows

#df[sample(nrow(df), 5), ]

#preds <- c(1, 2, 3)
#actual <- c(2, 2, 4)
#rss <- sum((preds - actual) ^ 2)  ## residual sum of squares
#tss <- sum((actual - mean(actual)) ^ 2)  ## total sum of squares
#rsq <- 1 - rss/tss
