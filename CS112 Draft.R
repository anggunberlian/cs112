date <- 12
indices_where_empty <- which(foo[date] == "")
foo[indices_where_empty, date] <- "NA"

foo[date] == ""
which(foo[date] == "")
foo[date]

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