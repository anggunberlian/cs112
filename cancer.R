library(Matching)
genout <- GenMatch(Tr=treat, X=X)

summary(mout)

mb <- MatchBalance(treat~age +educ+black+ hisp+ married+ nodegr+ u74+ u75+
                     re75+ re74+ I(re74*re75) + re78,
                   match.out=genout, nboots=500)

?GenMatch


label <- c(0, 0, 0, 0, 0, 1, 1, 1, 1, 1)
pred <- c(1, 1.5, 1, 2, 3.5, 2.5, 2.9, 3.9, 5, 6)
plot(pred, label)



glm.cancer <- glm(label ~ pred, family=binomial)
predict(glm.cancer)
?predict


plot(pred, label)
lines(pred, glm.cancer$fitted.values))

exp(glm.cancer$fitted.values) / (1 + exp(glm.cancer$fitted.values))

lm.cancer <- lm(label ~ pred)
plot(pred, label)
lines(pred, lm.cancer$fitted.values)


