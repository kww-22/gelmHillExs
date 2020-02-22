# #######################
# Gelman Hill Exercises
# Chapter 3
#########################

library(arm)

### Page 44

fit.2 <- lm(kid_score ~ mom_iq, data = kidiq)

display(fit.2)

fit.2.sim <- sim(fit.2)

plot(x = kidiq$mom_iq, y = kidiq$kid_score,
     xlab = "Mother IQ",
     ylab = "Kid Score")


for(i in 1:100){
  
  curve(expr = fit.2.sim@coef[i,1] + fit.2.sim@coef[i,2]*x, add = TRUE, col = "grey")

  }

curve(expr = coef(fit.2)[1] + coef(fit.2)[2]*x, add = TRUE, col = "black")

### Page 44-5

fit.3 <- lm(kid_score ~ mom_hs + mom_iq, data = kidiq)
beta.hat <- coef(fit.3)
beta.sim <- sim(fit.3)@coef

par(mfrow = c(1,2))
plot(x = kidiq$mom_iq, y = kidiq$kid_score,
     xlab = "Mom IQ", ylab = "Kid Score")

for (i in 1:100) {
  curve(cbind(1, mean(kidiq$mom_hs), x) %*% beta.sim[i,], col = "grey", add = TRUE)
}

curve(cbind(1, mean(kidiq$mom_hs), x) %*% beta.hat, col = "black", add = TRUE)

plot(x = kidiq$mom_hs, y = kidiq$kid_score, 
     xlab = "Mom Completed High School", ylab = "")


for (i in 1:100) {
  curve(cbind(1, x, mean(kidiq$mom_iq)) %*% beta.sim[i,], col = "grey", add = TRUE)
}

curve(cbind(1, x, mean(kidiq$mom_iq)) %*% beta.hat, col = "black", add = TRUE)

# Diagnostic predicted vs. residual plot
par(mfrow = c(1,1))
plot(predict(fit.3), resid(fit.3))


### Page 48
x.new <- data.frame(mom_hs = 1, mom_iq = 100)
predict(fit.3, x.new, interval = "prediction", level = 0.95)

### Chapter 3 exercises (pages 49-51)

exercise3.9.1 <- read.csv("C:/Users/kww0009/Box/R/GelmanHill_exercises/ARM_Data/pyth/exercise3.9.1.dat", sep="")

# Part A

mod.data <- exercise3.9.1[1:40,]
summary.data.frame(mod.data)

mod <- lm(y ~ x1 + x2, data = mod.data)
display(mod)


## Summarize model inferences and check model fit
### R-squared = 0.97; 97% of outcome varaince is accounted for by a linear comination of x1 and x2
### Residual standard deviation = 0.90; the mean prediction error of the model is 0.9 units
### Mean(y) = 13.9; residual sd = 6.5% of mean(y)
### Intercept coef = 1.32; when x1 & x2 = 0, y = 1.32
### x1 coef = 0.51; for every 1-unit increase in x1, the model would predict a 0.51-unit increase in y (assuming x2 is constant)
### x2 coef = 0.81; for every 1-unit increase in x1, the model would predict a 0.81-unit increase in y (assuming x1 is constant)

# Part B (plot the data with regression line)


plot(x = new.data$x2, y = new.data$y,
     xlab = "X2", ylab = "Y")

curve(cbind(1, mean(new.data$x1), x) %*% mod$coefficients, col = "black", add = TRUE)

# Part c (make a resiual plot)

resids <- resid(mod)
preds <- predict(mod)
sig <- sigma.hat(mod)

residual.plot(Expected = preds, Residuals = resids, sigma = sig,
              main = "Residual Plot (exercise 3.9.1 part D)")


# Pard D (make prediction for the remaining 20 data points)

predict.data <- exercise3.9.1[41:60,]

predict(mod, predict.data, interval = "prediction", level = 0.95)


# Exercise 2


