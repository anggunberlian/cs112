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

for (date in date.columns) {
  indices_where_empty <- which(foo[date] == "")
  foo[indices_where_empty, date] <- NA
  foo[[date]] <- as.Date(foo[[date]], format="%Y-%m-%d")
}

date <- 12
indices_where_empty <- which(foo[date] == "")
foo[indices_where_empty, date] <- "NA"

foo[date] == ""
which(foo[date] == "")
foo[date]

number_of_calls_per_day <- c(5, 2, 90, 45, 67, 90, 69, 2, 4, 5, 30)
larger_twenty <- number_of_calls_per_day[which(number_of_calls_per_day > 20)]

larger_twenty
less_twenty <- number_of_calls_per_day[-which(number_of_calls_per_day > 20)]
less_twenty
less_twenty2 <- number_of_calls_per_day[which(number_of_calls_per_day <= 20)]
less_twenty2
number_of_calls_per_day

head(foo)
foo[1, 12]
class(foo[1, 12])
class(foo[1, 22])

class(foo$AgreementDate)
date.columns
head(foo, 10)

# alright, hb this
foo[11, 12, 14, 15, 16, 17, 18, 25]
foo[11:12, 14:18, 25]
foo[date.columns]
na_assigned <- foo[date.columns], na.strings=c("", "", "NA")
na_assigned <- foo[date.columns, na.strings=date.columns("", "", "NA")]
sum(is.na(foo$CirculationDate[indices_2009]))

# boolean -- integer TRUE == 1, FALSE == 0
CD_correct_dates <- indices_2009[-which(foo$CirculationDate < 2009-01-01)]
CD_correct_dates
foo[4043, ]

y <- as.numeric((new_foo$RevisedCompletionDate - new_foo$ApprovalDate) / 30) # the delays in month
x <- new_foo$CirculationDate
plot(x, y, xlab = "Circulation date (year)", ylab = "Average delay (months)")
