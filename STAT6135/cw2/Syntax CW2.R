###############################################
#
#   STAT6135 GENERALISED LINEAR MODELS
#                 CW 2
#
###############################################

#######################
#READING THE DATASET

data.reg <- read.table("C:/Documents/STAT6135/session1.dat", header=T)
head(data.reg)

class(data.reg$sex)
sapply(data.reg, class)

data.reg$school <- factor(data.reg$school, levels=c(1,2,3))
data.reg$sex <- factor(data.reg$sex, levels=c(0,1), labels=c("Boy","Girl"))
data.reg$eg <- factor(data.reg$eg, levels=c(0,1), labels=c("White","Afr-Car"))

sapply(data.reg, class)
head(data.reg)

attach(data.reg)

#######################
#REGRESSION ANALYSIS

?lm

####MODEL 1
model.empty <- lm(mathatt2~1)
summary(model.empty)

####MODEL 2
model.curric <- lm(mathatt2~curric)
anova(model.curric)
summary(model.curric)

plot(curric, mathatt2)
abline(model.curric, col='red')

#Predicted values and residuals
model.curric$fitted # Fitted (predicted) values
resid(model.curric) # Regression (model) residuals
rstandard(model.curric) # Standardised residuals

library(MASS)
studres(model.curric) # Studentised residuals
plot(rstandard(model.curric), studres(model.curric))
abline(0,1)

qqnorm(rstandard(model.curric))
qqline(rstandard(model.curric)) # This adds the line on which the empirical 							
	# residuals should follow for normality to hold.

plot(model.curric$fitted, resid(model.curric)) # Raw residuals
abline(h=0) # Adds a constant line at 0.								

plot(model.curric$fitted, rstandard(model.curric)) # Studentised residuals
abline(h=0)			

####MODEL 3
model.mult <- lm(mathatt2 ~ curric + mathatt1)
anova(model.mult)
summary(model.mult)

####MODEL 4
math.quadr <- mathatt1^2
model.quadr <- lm(mathatt2~curric + mathatt1 + math.quadr)

####MODEL 5
model.gender <- lm(mathatt2~sex)
summary(model.gender)

####MODEL 7
model.int <- lm(mathatt2~curric + mathatt1 + sex * mathatt1)


#######################
#ANSCOMBE QUARTET

attach(anscombe)  # Attach the Anscombe data frame
anscombe  # View the Anscombe data frame
pairs(anscombe)  # Scatterplot matrix

par(mfrow=c(2,2)) # Define a 2 by 2 multifigure display - filled by rows
plot(x1, y1)
plot(x2, y2)
plot(x3, y3)
plot(x4, y4)

# Descriptive statistics
apply(anscombe, 2, mean) # Means
apply(anscombe, 2, var) # Variances
cbind(cor(x1,y1), cor(x2,y2), cor(x3,y3), cor(x4,y4)) # Correlations

# Fitting the regressions
f1 <- lm(y1 ~ x1) # Fit linear model: y1 regressed on x1
f2 <- lm(y2 ~ x2) # Fit linear model: y2 regressed on x2
f3 <- lm(y3 ~ x3) # Fit linear model: y3 regressed on x3
f4 <- lm(y4 ~ x4) # Fit linear model: y4 regressed on x4

# Regression coefficients
rbind(f1$coefficients, f2$coefficients, f3$coefficients, f4$coefficients)

# R-squared
rbind(summary(f1)$r.squared, summary(f2)$r.squared, summary(f3)$r.squared,
      summary(f1)$r.squared)

# Diagnostic plots. Run each block

# Pair 1
par(mfrow=c(2, 2)) # Define 1 by 2 multifigure display - filled by rows
plot(x1, y1)
abline(f1, col=2) # Add the linear regression line. col=2 corresponds to red
plot(x1, resid(f1))
qqnorm(resid(f1)) # A normal QQ plot to check for skewness, etc.
qqline(resid(f1)) # Adds a line to normal QQ plot
plot.new()

plot(f1)

#Pair 2
plot(x2, y2)
abline(f2, col=2) # Add the linear regression line
plot(x2, resid(f2))
qqnorm(resid(f2)) # A normal QQ plot to check for skewness, etc.
qqline(resid(f2)) # Add a line to normal QQ plot
plot.new()

plot(f2)

#Pair 3
plot(x3, y3)
abline(f3, col=2) # Add the linear regression line
plot(x3, resid(f3))
qqnorm(resid(f3)) # A normal QQ plot to check for skewness, etc.
qqline(resid(f3)) # Add a line to normal QQ plot
plot.new()

plot(f3)

#Pair 4
plot (x4, y4)
abline(f4, col=2) # Add the linear regression line
plot(x4, resid(f4))
qqnorm(resid(f4)) # A normal QQ plot to check for skewness, etc.
qqline(resid(f4)) # Add a line to normal QQ plot
plot.new()

plot(f4)


