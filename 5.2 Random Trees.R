setwd("/Users/anggunberlian/Desktop/CS112")
install.packages("tree")
library(tree)
library(ISLR)
attach(Carseats)
High=ifelse(Sales <=8,"No","Yes")
Carseats = data.frame(Carseats, High)
tree.carseats = tree(High ~ Sales, Carseats )
summary(tree.carseats)
plot(tree.carseats)
text(tree.carseats ,pretty =0)
tree.carseats
set.seed (2)
train=sample(1:nrow(Carseats), 200)
Carseats.test=Carseats [-train ,]
High.test=High[-train]
tree.carseats=tree(High~ Sales,Carseats,subset=train)
tree.pred=predict(tree.carseats,Carseats.test,type="class")
table(tree.pred ,High.test)
set.seed(3)
cv.carseats =cv.tree(tree.carseats, FUN = prune.misclass)
names(cv.carseats )
cv.carseats
par(mfrow=c(1,2))
plot(cv.carseats$size ,cv.carseats$dev ,type="b")
plot(cv.carseats$k ,cv.carseats$dev ,type="b")
prune.carseats=prune.misclass(tree.carseats,best=9)
> plot(prune.carseats )
> text(prune.carseats,pretty=0)

# CODE BELOW: 
# RUN AT THE BEGINNING TO LOAD FUNCTIONS IN MEMORY. 
corr.lever <- function()
{
  return(c(
    rep(rbinom(n = 1, size = 1, prob = 0.5), 10),
    rep(rbinom(n = 1, size = 1, prob = 0.5), 10),
    rep(rbinom(n = 1, size = 1, prob = 0.5), 10),
    rep(rbinom(n = 1, size = 1, prob = 0.5), 10),
    rep(rbinom(n = 1, size = 1, prob = 0.5), 10)))
}

# "PULL THE *UNCORR* LEVER"
# uncorrelated data
uncorr.lever <- function()
{
  return(rbinom(n = 50, size = 1, prob = 0.5))
}


# CONSIDER THE FOLLOWING GAME...
# IMAGINE YOU WANT TO ESTIMATE THE UNDERLYING PARAMETER VALUE
# OF A BERNOULLI DISTRIBUTION (i.e., THE PROBABILITY OF A "1"
# INSTEAD OF A "0"...)

# YOU CAN PULL THE "CORR" LEVER AND OBTAIN 50 CORRELATED DATA 
# POINTS FROM THE BERNOULLI DISTRIBUTION

# OR YOU CAN PULL THE "UNCORR" LEVER AND OBTAIN 50 UNCORRELATED DATA
# POINTS FROM THE BERNOULLI DISTRIBUTION.

# NOTICE THAT BOTH LEVERS PRODUCE RESULTS THAT HAVE THE SAME 
# EXPECTED VALUE.

# WHICH IS A BETTER CHOICE (TO LEARN ABOUT THE TRUE PARAMETER VALUE)?
corr.lever()
uncorr.lever()

plot(density(corr.lever()))
plot(density(uncorr.lever()))


# For the correlated lever, there are only 6 different correlations possible. 
yy <- c()
for(i in 1:10000) {yy[i] <- mean(corr.lever())}
hist(yy)


# Imagine a “coin flip RCT” to determine the coin’s true probability of heads.
# Flip the coin 1000 times, and take the average (1 vs. 0). What happens?

## Correlated coin:
aa <- corr.lever(); plot(density(aa)); abline(v = mean(aa), col = "red")
bb <- uncorr.lever(); plot(density(bb)); abline(v = mean(bb), col = "blue")

for(i in 1:1000) {abline(v = mean(corr.lever()), col = "red")}
for(i in 1:1000) {abline(v = jitter(mean(corr.lever())), col = "red")}

## Uncorrelated coin:
for(i in 1:1000) {abline(v = jitter(mean(uncorr.lever())), col = "blue")}

## IF YOU HAVE EXTRA TIME:
# run 1000 experiments (simulated trials)
for(i in 1:1000)
{
  storage.corr[i] <- mean(corr.lever())
}

for(i in 1:1000)
{
  storage.uncorr[i] <- mean(uncorr.lever())
}

# Was the standard deviation of the means derived from uncorrelated data
# LOWER than the standard deviation of the means derived from correlated data?
# Across all the simulated trials (for the whole shebang)?
sd(storage.uncorr) < sd(storage.corr)

# WHICH LEVER GIVES YOU BETTER INFORMATION?
# WHY?

