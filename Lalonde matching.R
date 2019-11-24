# PART I

Using the ‘lalonde’ data from the Matching library, calculate the ATE = ATT (difference in mean outcomes, treatment group MINUS control group)...

# PART II
# The fake observational Lalonde data is here: 

fake<- read.csv("https://tinyurl.com/yxzo52ez")

# Check it out... type: 
dim(fake)

library(Matching)

?Match

# Execute the documentation code EXCEPT RUN IT ON YOUR “fake” DATA, NOT “lalonde”. 

# NOTE THE TREATMENT EFFECT ESTIMATE. How does it compare to the “true” benchmark estimate from Part I? What’s the p-value?

# THEN--change the propensity score model dramatically, several different times, and see if the treatment effect changes. 

# Each time you try a different propensity score model, check the “balance” over the underlying variables you are trying to control for using the MatchBalance() function… How is the “smallest p-value after matching” changing? Keep track of your results in the Google Doc.


# Replication of Dehejia and Wahba psid3 model
#
# Dehejia, Rajeev and Sadek Wahba. 1999.``Causal Effects in
# Non-Experimental Studies: Re-Evaluating the Evaluation of Training
# Programs.''Journal of the American Statistical Association 94 (448):
# 1053-1062.

data(lalonde)

#
# Estimate the propensity model
#
glm1  <- glm(treat~age + I(age^2) + educ + I(educ^2) + black +
               hisp + married + nodegr + re74  + I(re74^2) + re75 + I(re75^2) +
               u74 + u75, family=binomial, data=fake)


#save data objects
#
X  <- glm1$fitted
Y  <- lalonde$re78
Tr  <- lalonde$treat

#
# one-to-one matching with replacement (the "M=1" option).
# Estimating the treatment effect on the treated (the "estimand" option defaults to ATT).
#
rr  <- Match(Y=Y, Tr=Tr, X=X, M=1);
summary(rr)

# Let's check the covariate balance
# 'nboots' is set to small values in the interest of speed.
# Please increase to at least 500 each for publication quality p-values.  
mb  <- MatchBalance(treat~age + I(age^2) + educ + I(educ^2) + black +
                      hisp + married + nodegr + re74  + I(re74^2) + re75 + I(re75^2) +
                      u74 + u75, data=lalonde, match.out=rr, nboots=10)