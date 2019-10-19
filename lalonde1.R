setwd("/Users/anggunberlian/Desktop/CS112")
#(1) Complete the lab work at the end of the chapter.
#(2) Bootstrap the 95% confidence interval for the treatment effect in the lalonde experiment--
#resample re78 from the treatment group (and take the mean), 
#resample re78 from the control group (and take the mean), 
#and then take the difference of means. Over and over again. 
#And store the results. 
#AND THEN, use the quantile function on that storage vector to obtain the 95% confidence interval.
library(Matching)
library(boot)
data(lalonde)
lalonde
lalonde$treat
indices_treat <- which(lalonde$treat == 1)
indices_treat
indices_con <- which(lalonde$treat == 0)
lalonde.treat <- lalonde[indices_treat,]
lalonde.con <- lalonde[indices_con,]

boot.fn <- function(data, index) return(mean(data$re78[index]))
boot.treat <- boot(lalonde.treat, boot.fn, 10000)
boot.con <- boot(lalonde.con, boot.fn, 10000)

boot.treat$t - boot.con$t
hist(boot.treat$t)
c_i_95 <- quantile(boot.treat$t, probs = seq(0.05, 0.95, by=0.1),CI.type = "two.sided", 
         CI.level = 0.75)
c_i_95
library(Matching)

data(lalonde)

foo <- lalonde

treated <- which(foo$treat == 1)
control <- which(foo$treat == 0)

storage <- NA

for (i in 1:100000)
{
  treated_Ys <- lalonde$re78[sample(treated,
                                    length(treated),
                                    replace = TRUE)]
  
  control_Ys <- lalonde$re78[sample(control,
                                    length(control),
                                    replace = TRUE)]
  
  storage[i] <- mean(treated_Ys) - mean(control_Ys)
}

#mean(storage)

# CONFINT
quantile(storage, probs = c(0.025, 0.975))
quantile(storage, probs = c(0.125, 0.875)) #75%

