

data.dir <- "Please put the directory of the data file"

Asphalt <- read.table(file.path(data.dir, "Please put the filename here"),
                      header=TRUE, sep="\t")
colnames(Asphalt) <- c("OBS", "y", paste("x", 1:6, sep=""))


## log-transform x1
Asphalt$logx1 <- log(Asphalt$x1)
pairs(Asphalt[, -c(1, 3)], main="Scatter plot matrix")

full.fit <- lm(y~logx1+x2+x3+as.factor(x4)+x5+x6, data=Asphalt)
summary(full.fit)


# Residuals vs the fitted values (Model Diagnostics)
r <- rstandard(full.fit)
plot(full.fit$fitted, r, xlab="Fitted Value", ylab="Standardized Residuals",
     main="Residuals Versus Fitted Values", pch=19)
abline(h=0, col="red")


#Normality asumption
qqnorm(r, cex=1, pch=19)
qqline(r, col="red")# Checking model adequacy

#A very clear non-linear pattern and some potential influential points.
#Approximately normal, but a potential influential point.

#Correlation
cor(Asphalt[, -(1:3)])



#Eigenvalues and condition numbers for leverage points
x <- model.matrix(full.fit)[, -1]
e <- eigen(t(x)%*%x)

kappa <- sqrt(e$val[1]/e$val)
kappa>30


vif(full.fit)



# Influential point detection
cook <- cooks.distance(full.fit)
max(cook)




# transform y to log(y).
Asphalt$logy <- log(Asphalt$y)
log.fit <- lm(logy~logx1+x2+x3+x4+x5+x6, Asphalt)
summary(log.fit)



# Q-Q plot

t <- rstandard(log.fit)
qqnorm(t, cex=1, pch=19)
qqline(t, col="red")

#seems fine !!!!




# Residuals vs the fitted values
plot(log.fit$fitted, t, xlab="Fitted Value", ylab="Studentized Residuals",
     main="Residuals Versus Fitted Values", pch=19)
abline(h=0, col="red")
#2 clusters of data... ugh



# 1. Sampe correlation coefficient
cor(Asphalt[, -c(1:3, 10)])



#Eigenvalues and condition numbers for leverage points
x <- model.matrix(full.fit)[, -1]
e <- eigen(t(x)%*%x)

kappa <- sqrt(e$val[1]/e$val)
kappa>30


vif(log.fit)


#NOOO colinearity remains


#Let's do model selection

library(MASS)
base <- lm(logy~1, data=Asphalt)
step <- stepAIC(base, list(upper=log.fit), direction="forward")


final.fit <- lm(logy~logx1+x2+x6, data=Asphalt)
summary(final.fit)


t.final <- rstudent(final.fit)
qqnorm(t.final, cex=1, pch=19)
qqline(t.final, col="red")


# Residuals vs the fitted values
plot(final.fit$fitted, t.final, xlab="Fitted Value", ylab="Residuals",
     main="Residuals Versus Fitted Values", pch=19)
abline(h=0, col="red")


# Identify influential data points
cook.final <- cooks.distance(final.fit)
summary(cook.final)


#Yes.. Improvements!!!!