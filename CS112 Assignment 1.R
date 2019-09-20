setwd("/Users/anggunberlian/Desktop/CS112")
### Multilateral Development Institution Data
foo <- read.csv("https://tinyurl.com/yb4phxx8") # read in the data
# column names
names(foo)
# dimensions of the data set
dim(foo)
# quick look at the data structure
head(foo)
# take note of the columns representing calendar dates
date.columns <- c(11, 12, 14, 15, 16, 17, 18, 25)

for(i in date.columns) # loops through the "date.columns"
{
  # Find missing values
  which_values_are_missing <- which(as.character(foo[, i]) == "")
  # Replace them by NAs
  foo[which_values_are_missing , i] <- NA
  # Turn values into dates
  foo[, i] <- as.Date(as.character(foo[, i]))
}

# Try this yourself
foo[3,12]
# [1] "1968 -03 -13"
foo[4,12]
# [1] "1968 -07 -03"
foo [3 ,12] - foo [4 ,12]

# Check out the class of CirculationDate and 2009-01-01
class(foo$CirculationDate)
class(2009-01-01)

#  Find the data where we do not have NAs and the date is more than or equal to 2009
indices_2009 <- which(!is.na(foo$CirculationDate) &
                      foo$CirculationDate >= as.Date("2009-01-01", '%Y-%m-%d'))

# Our data now
foo$CirculationDate[indices_2009]
indices_2009
foo$CirculationDate[indices_2009]
new_foo <- foo[indices_2009, ]

## ASSIGNMENT ##

# 1a. Is it true that the difference between Original Project Completion date and the Approval Date is (supposedly) approximately 24 months?
different_OCP_AP <- (new_foo$OriginalCompletionDate - new_foo$ApprovalDate) # num of pair-wise day difference
avg_days_OCP_AP <- mean(different_OCP_AP, na.rm = TRUE) # average days needed from Original Project Completion to approval
avg_month_OCP_AP <- as.numeric(avg_days_OCP_AP / 30) # average months needed
median_days_OCP_AP <- median(different_OCP_AP, na.rm = TRUE)
median_month_OCP_AP <- as.numeric(median_days_OCP_AP / 30)
avg_month_OCP_AP
median_month_OCP_AP

# 1a. Is it true that the project completion can be extended?
# 1a. Is it true that the difference between Revised Project Completion date & the Approval Date is larger than the difference between Original Project Completion date and the Approval Date?
different_RPC_AP <- (new_foo$RevisedCompletionDate - new_foo$ApprovalDate)
avg_days_RPC_AP <- mean(different_RPC_AP, n.rm=TRUE) # average days needed from Revised Project Completion to Approval, ignoring the NAs
avg_month_RPC_AP <- as.numeric(avg_days_RPC_AP / 30) # average months needed
avg_month_RPC_AP

avg_month_RPC_AP > avg_month_OCP_AP # checking if the difference between RPC and AP is indeed larger than the difference between OC and AP

# 1b. Has the length of project delay, measured as the difference between “OriginalCompletionDate” and “RevisedCompletionDate”, changed over time (consider projects circu- lated earlier and circulated later)?
y_delay <- as.numeric((new_foo$RevisedCompletionDate - new_foo$OriginalCompletionDate) / 30) # the delays in month
x <- new_foo$CirculationDate
plot(x, y_delay, xlab = "Circulation date (year)", ylab = "Average delay (months)", main = "The Plot of Length of Project Delay Overtime")

mean(y_delay, na.rm = TRUE) # na.rm -> to remove NA
median(y_delay, na.rm = TRUE)
quantile(y_delay, na.rm = TRUE)

# 1c. original planned project duration vs. actual duration
plot(density((as.numeric(different_OCP_AP[-which(is.na(different_OCP_AP))]) / 30)), 
      col = "red", lwd = 3, main = "Density Plot of The Planned vs. Actual Project Duration", xlab = "Duration (in months)")
lines(density((as.numeric(different_RPC_AP) / 30) ), col = "blue", lwd = 3 )

# stats for original planned project duration
mean_OCP_AP <- mean(as.numeric(different_OCP_AP[-which(is.na(different_OCP_AP))]) / 30)
mean_OCP_AP
median_OCP_AP <- median(as.numeric(different_OCP_AP[-which(is.na(different_OCP_AP))]) / 30)
median_OCP_AP
quantile_OCP_AP <- quantile(as.numeric(different_OCP_AP[-which(is.na(different_OCP_AP))]) / 30)
quantile_OCP_AP

# stats for actual planned project duration
mean_RPC_AP <- mean(as.numeric(different_RPC_AP) / 30)
mean_RPC_AP
median_RPC_AP <- median(as.numeric(different_RPC_AP) / 30)
median_RPC_AP
quantile_RPC_AP <- quantile(as.numeric(different_RPC_AP) / 30)
quantile_RPC_AP


# 2a. What % of projects completed between 2010 and now were rated 0?
sum(is.na(new_foo$RevisedCompletionDate) & !is.na(new_foo$OriginalCompletionDate)) 

# Data completed between 2010 and now
more_2010_revised_date <- which(!is.na(new_foo$RevisedCompletionDate) &
                  new_foo$RevisedCompletionDate >= as.Date("2010-01-01", '%Y-%m-%d'))

sum(more_2010_revised_date)
rating_zero <- sum(!is.na(new_foo$Rating[more_2010_revised_date]) &
                   new_foo$Rating[more_2010_revised_date]== 0)

rating_zero_percentage <- (rating_zero / length(more_2010_revised_date)) * 100
rating_zero_percentage

rating_one <- sum(!is.na(new_foo$Rating[more_2010_revised_date]) &
                    new_foo$Rating[more_2010_revised_date]== 1)

rating_one_percentage <- (rating_one / length(more_2010_revised_date)) * 100
rating_one_percentage

rating_two <- sum(!is.na(new_foo$Rating[more_2010_revised_date]) &
                    new_foo$Rating[more_2010_revised_date]== 2)

rating_two_percentage <- (rating_two / length(more_2010_revised_date)) * 100
rating_two_percentage

rating_three <- sum(!is.na(new_foo$Rating[more_2010_revised_date]) &
                     new_foo$Rating[more_2010_revised_date]== 3)

rating_three_percentage <- (rating_three / length(more_2010_revised_date)) * 100
rating_three_percentage

# 3. Repeat problem 2, but this time limit your analysis purely to policy and advisory technical assistance (”PATA”) projects.

PATA_only <- new_foo$Type == "PATA"

sum(PATA_only)

rating_0_PATA <- sum(!is.na(new_foo$Rating[PATA_only]) &
                    new_foo$Rating[PATA_only] == 0)
rating_0_PATA
rating_0_PATA_percentage <- (rating_0_PATA / length(PATA_only)) * 100
rating_0_PATA_percentage

rating_1_PATA <- sum(!is.na(new_foo$Rating[PATA_only]) &
                       new_foo$Rating[PATA_only] == 1)
rating_1_PATA
rating_1_PATA_percentage <- (rating_1_PATA / length(PATA_only)) * 100
rating_1_PATA_percentage

rating_2_PATA <- sum(!is.na(new_foo$Rating[PATA_only]) &
                       new_foo$Rating[PATA_only] == 2)

rating_2_PATA_percentage <- (rating_2_PATA / length(PATA_only)) * 100
rating_2_PATA_percentage

rating_3_PATA <- sum(!is.na(new_foo$Rating[PATA_only]) &
                       new_foo$Rating[PATA_only] == 3)
rating_3_PATA

rating_3_PATA_percentage <- (rating_3_PATA / length(PATA_only)) * 100
rating_3_PATA_percentage

# 4. Top 10% of projects by “Revised.Amount” vs the bottom 10% of projects by “RevisedAmount”
# Create deciles based on the values of the vector


quantile_10_90 <- quantile(new_foo$RevisedAmount, probs = c(0.1, 0.9), na.rm = FALSE) # like a vector

plot(density(new_foo$RevisedAmount), col = "red", lwd = 3, main = "Density Plot of Revised Amount")

top_10 <- which(new_foo$RevisedAmount >= quantile_10_90[2])

bottom_10 <- which(new_foo$RevisedAmount <= quantile_10_90[1])



plot(density(new_foo$Rating[top_10]), col = "red", lwd = 3, main = "Distribution Plot of Top & Bottom 10% Revised Amount", xlab = "Rating", ylim = c(0, 2))
lines(density(new_foo$Rating[bottom_10], na.rm = TRUE), col = "blue", lwd = 3 )

table_DDCC <- table(c(replicate(length(new_foo$Dept[top_10]), 0),
                      replicate(length(new_foo$Dept[bottom_10]), 1)),
                    c(new_foo$Dept[top_10], new_foo$Dept[bottom_10]))
colnames(table_DDCC) = levels(new_foo$Dept)[as.numeric(colnames(table_DDCC))]

# A bar plot of Department for top and bottom 10%

barplot(table_DDCC, legend = c("Top 10%", "Bottom 10%"), beside = TRUE,
        las=2, args.legend = list(x="topleft"))

