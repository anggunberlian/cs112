install.packages("Matching")
library(Matching)
# 2. Load the lalonde data set into working memory. 
data(lalonde)
# 3. What are the dimensions of the data set?
dim(lalonde)
# 4. What are the names of the columns?
colnames(lalonde)
# 5. How many different variable types are represented in this data set?
dim(lalonde)[2]
lalonde[<all_rows>,2nd col]
lalonde[2]
summary(lalonde)
lalonde[2]
# 5. How many different variable types are represented in this data set?
### To see the different variable types:
for(i in 1:length(names(lalonde))) {cat( class(lalonde[,i]), "\n")}

### To count the unique types:
length(unique(sapply(lalonde, class)))

# 6. What's the maximum value of the re74 column? (re74 indicates the person's real earnings in 1974)
max(lalonde['re74'])
# 7. What's the minimum value of this column?
min(lalonde['re74'])
# 8. How many of the elements of this column are equal to zero?
sum(lalonde['re74'] == 0)
# 9. How many elements of this column are less than $15000 OR greater than $20000?
sum(lalonde['re74'] < 15000) | sum(lalonde['re74'] > 20000) #TURN OUT THIS WOULD COMPARE EHEHEHHE
sum(lalonde['re74'] < 15000 | (lalonde['re74'] > 20000)) #THIS IS KOREK
# 10. How many people in this data set are married, earned zero real earnings in 1978, and have more than 12 years of education?
sum((lalonde['married'] == 1) & (lalonde(['re78'] == 0) & (lalonde(['educ'] > 12))
sum((lalonde$married == 1) & (lalonde$educ > 12) & (lalonde$re78 ==0))
### MORE ADVANCED:

# 11. What is the interquartile range of "re78" (real earnings in 1978)? Use the "quantile()" function.
quantile(['re78'])
### upper-bound of the interquartile range:
quantile(lalonde$re78, 0.75)
quantile(lalonde['re78'], 0.75)

### lower-bound of the interquartile range:
quantile(lalonde$re78, 0.25)
quantile(lalonde['re78'], 0.25)
# 12. Create a scatterplot, with re74 on the x-axis, and re78 on the y-axis. Label the axes.
# Draw a regression line if you wish (and choose a fun color).

# 13. Make a function with a single argument (column number) that outputs the median of that column.
# Extra effort: if the user specifies a non-numeric column, then the function returns an error message.

# 14. Run a univariate regression, with "age" as a predictor (x variable), re75 as outcome (the "y").
# Interpret the 2 coefficients (of the intercept, and the x variable).

# 15. Run a regression with 2 predictors, "age" and "educ", with re75 as the outcome (the "y").
# Interpret the 3 coefficients (of the intercept, and the 2 x variables).
?datasets
library(datasets)
beaver2
library(datasets)
beaver2
?beaver2
sum(beaver2)
