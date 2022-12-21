
library(lattice)
library(tidyverse)
library(broom)
# Linear Regression with R

data_reg <- read.table("session1.dat",header=T)
sapply(data_reg, class)

data_reg$school <- factor(data_reg$school, levels=c(1,2,3))
data_reg$sex <- factor(data_reg$sex, levels=c(0,1), labels=c("Boy","Girl"))
data_reg$eg <- factor(data_reg$eg, levels=c(0,1), labels=c("White","Afr-Car"))

    # Model 1 -- empty model

model_empty <- lm(data_reg$mathatt2 ~ 1)
summary(model_empty)
    # Model 2 -- linear regresion, mathatt2 - curric
model_curric <- lm(data_reg$mathatt2 ~ data_reg$curric)
anova(model_curric)
summary(model_curric)

# Task 2:
#b) extracting std errors of model
tidy(model_curric)$std.error
#c) p-value (from F test) comparing null to curric model gives very significant 
    # result with p = 5.5e-06 therefore the regession line is significant
#d) Using R^2 to find proportion of variance explained by predictors
glance(model_curric)$adj.r.squared
#e)
tidy(model_curric)
#On average someone who has no curric score will have a math2 score of 13 (2 s.f.)
#A 10 point increase in curric will on average mean an increase in math2 score by 8.3 (2 s.f.) -- WHAT?



plot(data_reg$curric, data_reg$mathatt2)
abline(model_curric, col="red")

# Task 3:

model_curric$fitted # Fitted (predicted) Values
resid(model_curric) # Regression (model) residuals
rstandard(model_curric) # Studentised Residuals
resid(model_curric)/sigma(model_curric) # Compare to standarised residuals

ggplot(data = data_reg, 
    aes(x=curric, y=mathatt2))+geom_point()+geom_smooth(method="lm",
    formula= y~x)
    # hence; Confirm rstandard produces studentised residuals
resid(model_curric)/(sigma(model_curric)*sqrt(1-hatvalues(model_curric)))

# Checking Normality
qqnorm(rstandard(model_curric))
qqline(rstandard(model_curric))


plot(model_curric$fitted, resid(model_curric)) # Raw residuals
abline(h=0) # Adds a constant line at 0.


plot(model_curric$fitted, rstandard(model_curric)) # Studentised residuals
abline(h=0)

# Task 4

#There is no clear pattern in the residual plot, with points fairly well scatterd. 
# The points are fairly well centered around zero with no obvious homoscedatic trends (residuals become larger or the reverse)
# Thus linearuty assumption met

#As we only have 1 predictor we do not need to check that the predictors are independent with each other

#looking at the resdiual plot again,; the residual errors appear to have a mean value of zero (so this assumption met)


    # Model 3 -- linear regresion, mathatt2 - curric
model_multi <- lm(data_reg$mathatt2 ~ data_reg$curric + data_reg$mathatt1)
anova(model_multi)

# Task 5:
    # Is the model improved by mathatt 1?

AIC(model_curric) - AIC(model_multi)

# The newly fitted model has a lower AIC, therefore is a better fitting model using that as the selection criterion
# The newly added variable of mathatt1 is also hihgly significant 

model_multi$fitted # Fitted (predicted) Values
resid(model_multi) # Regression (model) residuals
rstandard(model_multi) # model_multi Residuals
resid(model_multi)/sigma(model_curric) # Compare to standarised residuals

# Checking Normality
qqnorm(rstandard(model_multi))
qqline(rstandard(model_multi))


plot(model_multi$fitted, resid(model_multi)) # Raw residuals
abline(h=0) # Adds a constant line at 0.


plot(model_multi$fitted, rstandard(model_multi)) # Studentised residuals
abline(h=0)

    # Model 3 -- linear regresion, mathatt2 - curric
math_quadr <- data_reg$mathatt1^2
model_quadr <- lm(data_reg$mathatt2 ~ data_reg$curric + 
                                    data_reg$mathatt1 +
                                    math_quadr)
anova(model_quadr)
summary(model_quadr)
anova(model_curric, model_multi, model_quadr)

# Task 6
AIC(model_quadr)
#  No it does not aic(model_quadr) > aic(model_multi)

qqnorm(rstandard(model_quadr))
qqline(rstandard(model_quadr))
plot(model_quadr$fitted, resid(model_quadr)) # Raw residuals
abline(h=0)


# Categorical Variables
    # Model 4
model_gender <- lm(mathatt2 ~ sex, data=data_reg)
summary(model_gender)

# Task 7
    # Model 5
model_5 <- lm(mathatt2 ~ curric + mathatt1 + math_quadr + sex)
summary(model_5)


# Interaction Variables 
    # Model 6
model_int <- lm(mathatt2 ~ curric + mathatt1 + sex * mathatt1)
summary(model_int)

# Task 8





# The Anscombre quartet example
attach(anscombe)  # Attach the Anscombe data frame
anscombe
pairs(anscombe)  # Scatterplot matrix

# Task 9
par(mfrow=c(2,2)) # Define a 2 by 2 multifigure display - filled by rows
plot(x1, y1) # Sure
plot(x2, y2) # Nope
plot(x3, y3) # Yes
plot(x4, y4) # Nope

apply(anscombe, 2, mean) # Means
apply(anscombe, 2, var) # Variances
cbind(cor(x1,y1), cor(x2,y2), cor(x3,y3), cor(x4,y4)) # Correlations

# Task 10:
    # Using these descriptive statistic is useless

# Fitting the regressions
f1 <- lm(y1 ~ x1) # Fit linear model: y1 regressed on x1
f2 <- lm(y2 ~ x2) # Fit linear model: y2 regressed on x2
f3 <- lm(y3 ~ x3) # Fit linear model: y3 regressed on x3
f4 <- lm(y4 ~ x4) # Fit linear model: y4 regressed on x4

# Regression coefficients
rbind(f1$coefficients, f2$coefficients, f3$coefficients, f4$coefficients)
rbind(summary(f1)$r.squared, summary(f2)$r.squared, summary(f3)$r.squared,
     summary(f1)$r.squared)


# Pair 1
par(mfrow=c(2, 2)) # Define 1 by 2 multifigure display - filled by rows
plot(x1, y1)
abline(f1, col=2) # Add the linear regression line. col=2 corresponds to red
plot(x1, resid(f1))
qqnorm(resid(f1)) # A normal QQ plot to check for skewness, etc.
qqline(resid(f1)) # Adds a line to normal QQ plot

par(mfrow=c(2,2))
plot(f1)

#Pair 2
par(mfrow=c(2,2)) # Define 2 by 2 multifigure display - filled by rows
plot(x2, y2)
abline(f2, col=2) # Add the linear regression line
plot(x2, resid(f2))
qqnorm(resid(f2)) # A normal QQ plot to check for skewness, etc.
qqline(resid(f2)) # Add a line to normal QQ plot

par(mfrow=c(2,2))
plot(f2)

#Pair 3
par(mfrow=c(2,2)) # Define 2 by 2 multifigure display - filled by rows
plot(x3, y3)
abline(f3, col=2) # Add the linear regression line
plot(x3, resid(f3))
qqnorm(resid(f3)) # A normal QQ plot to check for skewness, etc.
qqline(resid(f3)) # Add a line to normal QQ plot

par(mfrow=c(2,2))
plot(f3)

#Pair 4
par(mfrow=c(2,2)) # Define 2 by 2 multifigure display - filled by rows
plot (x4, y4)
abline(f4, col=2) # Add the linear regression line
plot(x4, resid(f4))
qqnorm(resid(f4)) # A normal QQ plot to check for skewness, etc.
qqline(resid(f4)) # Add a line to normal QQ plot

par(mfrow=c(2,2))
plot(f4)

# Task 11
library(car)
